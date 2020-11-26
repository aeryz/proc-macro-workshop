extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{Data, DeriveInput, Ident, parse_macro_input};
use quote::{quote};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let ident = &input.ident;
    let bname = format!("{}Builder", ident);
    let bident = Ident::new(&bname, ident.span());

    let ds = match &input.data {
        &Data::Struct(ref ds) => ds,
        _ => panic!("Unexpected"),
    };

    let fields = &ds.fields;

    struct AltField {
        is_opt: bool,
        ty: syn::Type,
        ident: Ident,
    }
    let mut alt_fields = Vec::new();

    for field in fields {
        let ident = field.ident.clone().unwrap();
        let mut ty = &field.ty;
        let mut is_optional = false;
        if let syn::Type::Path(
            syn::TypePath { 
                qself: None, 
                path: syn::Path { 
                    segments: punch,
                    ..
                }
            }
        ) = ty {
            if let Some(
                syn::PathSegment { 
                    ident, 
                    arguments: syn::PathArguments::AngleBracketed(
                        syn::AngleBracketedGenericArguments {
                            args: inner_punch,
                            ..
                        }
                    )
                }
            ) = punch.first() {
                if ident == "Option" {
                    if let Some(syn::GenericArgument::Type(inner_ty)) = inner_punch.first() {
                        is_optional = true;
                        ty = inner_ty;
                    }
                }
            }
        }

        alt_fields.push(AltField {
            is_opt: is_optional,
            ty: ty.clone(),
            ident: ident.clone(),
        });
    }

    let opt = alt_fields.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;
        quote! { #ident: std::option::Option<#ty> }
    });
    
    let none_opt = alt_fields.iter().map(|field| {
        let ident = &field.ident;
        quote!{ #ident: std::option::Option::None }
    });

    let setters = alt_fields.iter().map(|field | {
        let ident = &field.ident;
        let ty = &field.ty;
        quote! { 
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        }
    });


    let command_inner = alt_fields.iter().map(|field| {
        let ident = &field.ident;
        if field.is_opt {
            quote! { #ident: self.#ident.clone() } 
        } else {
            quote! {
                #ident: self.#ident.clone().ok_or("#ident cannot be None")?
            }
        }
    });

//     Type::Path(
//         TypePath {
//             qself: None,
//             path: Path {
//                 segments: [
//                     PathSegment {
//                         ident: "Option",
//                         arguments: PathArguments::AngleBracketed(
//                             AngleBracketedGenericArguments {
//                                 args: [
//                                     GenericArgument::Type(
//                                         ...
//                                     ),
//                                 ],
//                             },
//                         ),
//                     },
//                 ],
//             },
//         },
//     )

    let expanded = quote! {
        pub struct #bident {
            #(#opt,)*
        }

        impl #bident {
            #(#setters)*

            pub fn build(&self) -> Result<#ident, Box<dyn std::error::Error>> {
                Ok(#ident {
                    #(#command_inner,)*
                })
            }
        }

        impl #ident {
            pub fn builder() -> #bident {
                #bident {
                    #(#none_opt,)*
                }
            }
        }
    };

    let stream = proc_macro::TokenStream::from(expanded);
    println!("{}", stream);
    stream
}
