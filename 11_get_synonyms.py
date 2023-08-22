from gensim.models import KeyedVectors
import os
import pandas as pd

# Note: had to add header to .txt file download for reading with gensim

# # Determine vocab_size and vector_size
# with open('glove_vectors_arwiki.txt', 'r', encoding='utf-8') as f:
#     first_line = f.readline().split()
#     vector_size = len(first_line) - 1  # Subtract one for the word itself
#     vocab_size = 1 + sum(1 for _ in f)  # 1 for the first line we already read

# # Get corrected file with header
# with open('glove_vectors_arwiki.txt', 'w', encoding='utf-8') as f_out:
#     f_out.write(f"{vocab_size} {vector_size}\n")
#     with open('glove_vectors_arwiki.txt', 'r', encoding='utf-8') as f_in:
#         f_out.writelines(f_in)

# Now load using gensim
model = KeyedVectors.load_word2vec_format('data/pretrained_embedding/glove_vectors_arwiki.txt', binary=False)

def get_synonyms(word, topn=100):
    return [item[0] for item in model.most_similar(word, topn=topn)]

word1_synonyms = get_synonyms('المعارضة')
word2_synonyms = get_synonyms('الدعم')

oppterms = pd.DataFrame(word1_synonyms)
supterms = pd.DataFrame(word2_synonyms)

oppterms.to_csv('data/pretrained_embedding/oppterms.csv', index=False, encoding='utf-8-sig')
supterms.to_csv('data/pretrained_embedding/supterms.csv', index=False, encoding='utf-8-sig')

# then manually code