module Web.EmbedThis.Service where

data Service a b = Service (UrlParser a) (DataParser b)
