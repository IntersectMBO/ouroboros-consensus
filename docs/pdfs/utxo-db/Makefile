
all: utxo-db.pdf utxo-db-api.pdf

%.pdf: %.tex
	latexmk -bibtex -pdf $<

clean:
	latexmk -CA

# utxo-db-api.pdf: hstore-anticaching-fig1.pdf
