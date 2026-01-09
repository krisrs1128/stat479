"""
Based off

@inproceedings{tak2025mechanistic,
  title     = {Mechanistic Interpretability of Emotion Inference in Large Language Models},
  author    = {Tak, Ala N. and Banayeeanzade, Amin and Bolourani, Anahita and Kian, Mina and Jia, Robin and Gratch, Jonathan},
  booktitle = {Findings of the Association for Computational Linguistics: ACL 2025},
  month     = jul,
  year      = {2025},
  address   = {Vienna, Austria},
  publisher = {Association for Computational Linguistics},
  pages     = {13090--13120}
}
"""

import os
import pandas as pd
import torch
from torch.utils.data import DataLoader
from transformers import AutoTokenizer
from importlib import import_module

helpers = import_module("17-helpers")
MistralForCausalLM = helpers.MistralForCausalLM
from utils import TextDataset, extract_hidden_states


# Configuration
MODEL_NAME = 'mistralai/Ministral-8B-Instruct-2410'
MODEL_SHORT_NAME = 'Ministral-8B-Instruct-2410'
PROMPT_TYPE = 'joy_sadness_0'
BATCH_SIZE = 1
DEVICE_MAP = 'mps'

# Create output directory
os.makedirs('outputs/', exist_ok=True)

# Load and prepare data
train_data = pd.read_csv('data/enVent_gen_Data.csv', encoding='ISO-8859-1')
train_data['emotion'] = train_data['emotion'].replace('no-emotion', 'neutral')

# Define emotions and appraisals
emotions_list = [
    'anger', 'boredom', 'disgust', 'fear', 'guilt', 'joy', 'neutral',
    'pride', 'relief', 'sadness', 'shame', 'surprise', 'trust'
]

appraisals = [
    'predict_event', 'pleasantness', 'other_responsblt', 'chance_control',
    'suddenness', 'familiarity', 'unpleasantness', 'goal_relevance',
    'self_responsblt', 'predict_conseq', 'goal_support', 'urgency',
    'self_control', 'other_control', 'accept_conseq', 'standards',
    'social_norms', 'attention', 'not_consider', 'effort'
]

# Map emotions to IDs and create labels
emotion_to_id = {emotion: i for i, emotion in enumerate(emotions_list)}
train_data['emotion_id'] = train_data['emotion'].map(emotion_to_id).astype(int)

# Build prompts
if '_' in PROMPT_TYPE:
    shots = PROMPT_TYPE.split('_')[:-1]
    prompt_index = int(PROMPT_TYPE.split('_')[-1])
else:
    shots = []
    prompt_index = int(PROMPT_TYPE)

prompt_func = helpers.build_prompt(shots=shots, prompt_index=prompt_index)
train_data['input_text'] = train_data['hidden_emo_text'].apply(prompt_func)

# Create dataset and dataloader
labels = torch.from_numpy(train_data[['emotion_id'] + appraisals].to_numpy())
dataset = TextDataset(train_data['input_text'].tolist(), labels)
dataloader = DataLoader(dataset, batch_size=BATCH_SIZE, shuffle=False)

# Initialize tokenizer and model
os.makedirs(f'outputs/{MODEL_SHORT_NAME}', exist_ok=True)
tokenizer = AutoTokenizer.from_pretrained(MODEL_NAME, padding_side='left')
if tokenizer.pad_token is None:
    tokenizer.pad_token = tokenizer.eos_token

model = MistralForCausalLM.from_pretrained(MODEL_NAME, device_map=DEVICE_MAP)

# Extract attention weights
dataloader_1bs = DataLoader(dataset, batch_size=1, shuffle=False)
extraction_layers = list(range(model.config.num_hidden_layers))
extraction_locs = [10]
extraction_tokens = [-1]

results = extract_hidden_states(
    dataloader_1bs,
    tokenizer,
    model,
    extraction_locs=extraction_locs,
    extraction_layers=extraction_layers,
    extraction_tokens=extraction_tokens,
    do_final_cat=False,
    return_tokenized_input=True
)

# Save results
output_file = f'attention_weights_layers_{extraction_layers}_locs_{extraction_locs}_tokens_{extraction_tokens}.pt'
output_path = f'outputs/{MODEL_SHORT_NAME}/{output_file}'
torch.save(results, output_path)