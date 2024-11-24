from sentence_transformers import SentenceTransformer, util
import json
import pandas as pd
from langdetect import detect, DetectorFactory, LangDetectException
from more_itertools import chunked  # For batching

DetectorFactory.seed = 0
model = SentenceTransformer('paraphrase-MiniLM-L6-v2')

output_file = "coherence_scores.json"

reviews = pd.read_csv("reviews.csv", usecols=['book_id', 'text'])

reviews = reviews.dropna(subset=['text'])

reviews = reviews[reviews['text'].str.strip() != '']

total_reviews = len(reviews)  # Total number of reviews

# Filter out non-English reviews
def is_english(text):
    try:
        return detect(text) == 'en'
    except LangDetectException:
        return False
    
reviews['is_english'] = reviews['text'].apply(is_english)
reviews = reviews[reviews['is_english']]  # Keep only English reviews
reviews = reviews.drop(columns=['is_english'])

batch_size = 64  # Adjust based on available memory and model capacity
from tqdm import tqdm  # Import for progress bar

total_reviews = len(reviews)  # Total number of reviews
processed_reviews = 0  # Counter for processed reviews

# Open the output file and process in batches
with open(output_file, 'w') as f:
    for batch in tqdm(chunked(reviews.itertuples(index=False), batch_size), total=(total_reviews // batch_size) + 1, desc="Processing Batches"):
        # Collect sentences for batch
        batch_sentences = []
        book_ids = []
        for row in batch:
            book_ids.append(row.book_id)
            batch_sentences.extend(row.text.split('. '))

        # Compute embeddings in batch
        sentence_embeddings = model.encode(batch_sentences)

        # Process coherence for each review in the batch
        offset = 0
        for i, row in enumerate(batch):
            review_text = row.text
            sentences = review_text.split('. ')
            num_sentences = len(sentences)
            embeddings = sentence_embeddings[offset:offset + num_sentences]
            offset += num_sentences

            # Calculate coherence scores
            coherence_scores = [
                util.cos_sim(embeddings[i], embeddings[i+1]).item()
                for i in range(len(embeddings) - 1)
            ]
            average_coherence = sum(coherence_scores) / len(coherence_scores) if coherence_scores else 0.0

            # Save results
            result = {
                "book_id": row.book_id,
                "average_coherence": average_coherence
            }
            f.write(json.dumps(result) + '\n')

        # Update the count of processed reviews
        processed_reviews += len(batch)

        # Print progress (optional, only for additional visibility)
        if processed_reviews % (batch_size * 10) == 0:  # Every 10 batches
            progress = (processed_reviews / total_reviews) * 100
            print(f"Progress: {progress:.2f}% ({processed_reviews}/{total_reviews})")
