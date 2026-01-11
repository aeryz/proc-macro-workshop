use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, DataStruct, DeriveInput, Fields,
    FieldsNamed, Ident, Meta, MetaNameValue, Path, PathSegment, Type, TypePath,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let ident = input.ident;
    let builder_ident = Ident::new(&format!("{ident}Builder"), proc_macro2::Span::call_site());

    let syn::Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed {
            named: field_names, ..
        }),
        ..
    }) = input.data
    else {
        panic!("expected a struct");
    };

    let (fields_def, fields_set, field_setters, main_setters, additional_fns): (
        Vec<_>,
        Vec<_>,
        Vec<_>,
        Vec<_>,
        Vec<_>,
    ) = field_names
        .into_iter()
        .map(|f| {
            let has_each = f.attrs.iter().find_map(|a| {
                a.parse_args()
                let Meta::NameValue(MetaNameValue {
                    path: Path { segments, .. },
                    value,
                    ..
                }) = a.meta.clone()
                else {
                    panic!("not name value?")
                };

                if segments[0].clone().ident == "each" {
                    Some(value)
                } else {
                    None
                }
            });

            let name = f.ident.unwrap();
            let ty = f.ty;

            let Type::Path(TypePath {
                path: Path { segments, .. },
                ..
            }) = ty.clone()
            else {
                panic!("invalid");
            };

            let additional_fn = match has_each {
                Some(expr) => {
                    let syn::PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        args,
                        ..
                    }) = segments[0].clone().arguments
                    else {
                        panic!("expected angle brancketed");
                    };

                    quote! {
                        pub fn #expr(&mut self, item: #args) -> &mut Self {
                            if self.#name.is_none() {
                                self.#name = Some(vec![item]);
                            } else {
                                self.#name.as_mut().push(item)
                            }
                            self
                        }
                    }
                }
                None => quote! {},
            };

            let ty_optional = segments[0].clone().ident == "Option";

            if ty_optional {
                let syn::PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    args, ..
                }) = segments[0].clone().arguments
                else {
                    panic!("optional type");
                };
                (
                    quote!(
                        #name: #ty
                    ),
                    quote! {
                        #name: None
                    },
                    quote! {
                        pub fn #name(&mut self, #name: #args) -> &mut Self {
                            self.#name = Some(#name);
                            self
                        }
                    },
                    quote! {
                        #name: self.#name.clone()
                    },
                    additional_fn,
                )
            } else {
                (
                    quote! {
                        #name: Option<#ty>
                    },
                    quote! {
                        #name: None
                    },
                    quote! {
                        pub fn #name(&mut self, #name: #ty) -> &mut Self {
                            self.#name = Some(#name);
                            self
                        }
                    },
                    quote! {
                        #name: self.#name.clone().ok_or("error")?
                    },
                    additional_fn,
                )
            }
        })
        .collect();

    let q = quote! {
        pub struct #builder_ident {
            #(#fields_def),*
        }

        impl #builder_ident {
            pub fn build(&mut self) -> Result<#ident, Box<dyn ::core::error::Error>> {
                Ok(#ident {
                    #(#main_setters),*
                })
            }

            #(#field_setters)*

            #(#additional_fns)*
        }

        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#fields_set),*
                }
            }
        }
    };

    panic!("{q}");

    q.into()
}
