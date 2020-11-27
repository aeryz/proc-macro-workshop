extern crate proc_macro;

use proc_macro::{TokenStream};
use syn::{Data, DeriveInput, Ident, parse_macro_input};
use quote::{quote};

#[proc_macro_derive(Builder, attributes(builder))]
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

    struct BuilderAttr {
        inner_ty: syn::Type,
        method_name: String,
    }

    struct AltField {
        is_opt: bool,
        ty: syn::Type,
        ident: Ident,
        builder_attr: Option<BuilderAttr>,
    }
    let mut alt_fields = Vec::new();

    for field in fields {
        let ident = field.ident.clone().unwrap();
        let mut ty = &field.ty;
        let mut is_optional = false;
        let mut builder_attr = None;
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


                if ident == "Vec" && !field.attrs.is_empty() {
                    let inner_ty = if let syn::GenericArgument::Type(inner_ty) = inner_punch.first().unwrap() {
                        inner_ty
                    } else {
                        panic!("Nope");
                    };
                    let attr = field.attrs.first().unwrap();
                    let nested= if let syn::Meta::List(syn::MetaList { nested, .. }) = attr.parse_meta().unwrap() {
                        nested
                    } else {
                        panic!("Nope");
                    };
                    if let syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue { lit: syn::Lit::Str(lit_str), ..})) = nested.first().unwrap() {
                        builder_attr = Some(
                            BuilderAttr {
                                method_name: lit_str.value(),
                                inner_ty: inner_ty.clone(),
                            }
                        );
                    }
                }
            }
        }

        alt_fields.push(AltField {
            is_opt: is_optional,
            ty: ty.clone(),
            ident: ident.clone(),
            builder_attr
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
        let opt_ts = match field.builder_attr {
            Some(ref attr) => {
                let inner_ty = &attr.inner_ty;
                let method_name = Ident::new(&attr.method_name.as_str(), ident.span());
                Some(quote! {
                    fn #method_name(&mut self, #method_name: #inner_ty) -> &mut Self {
                        if self.#ident.is_some() {
                            self.#ident.as_mut().unwrap().push(#method_name);
                        } else {
                            self.#ident = Some(vec![#method_name]);
                        }
                        self
                    }
                })
            }, None => None
        };

        let setter_fn = quote! {
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        };

        if opt_ts.is_some() {
            let opt_ts = opt_ts.unwrap();
            if ident == &field.builder_attr.as_ref().unwrap().method_name {
                opt_ts
            } else {
                quote! {
                    #opt_ts
                    #setter_fn
                }
            }
        } else {
            setter_fn
        }
    });

    let a: Option<Vec<String>> = None;
    let b = a.unwrap_or(std::vec::Vec::new());

    let command_inner = alt_fields.iter().map(|field| {
        let ident = &field.ident;
        if field.is_opt {
            quote! { #ident: self.#ident.clone() } 
        } else if field.builder_attr.is_some() {
            quote! {
                #ident: self.#ident.clone().unwrap_or(std::vec::Vec::new())
            }
        } else {
            quote! {
                #ident: self.#ident.clone().ok_or(format!("{} cannot be None", stringify!(#ident)).as_str())?
            }
        }
    });

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
    stream
}
