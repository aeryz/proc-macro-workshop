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

    let opt = fields.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        let ty = &field.ty;
        quote! { #ident: std::option::Option<#ty> }
    });
    
    let none_opt = fields.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        quote!{ #ident: std::option::Option::None }
    });

    let setters = fields.iter().map(|field | {
        let ident = field.ident.as_ref().unwrap();
        let ty = &field.ty;
        quote! { 
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        }
    });

    let command_inner = fields.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        quote! {
            #ident: self.#ident.clone().ok_or("#ident cannot be None")?
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
    println!("{}", stream);
    stream
}
