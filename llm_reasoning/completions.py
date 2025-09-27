import openai
import os
import random
from threading import Thread
from queue import Queue

# Get API key from environment variable
openai.api_key = os.getenv("OPENAI_API_KEY")
if not openai.api_key:
    raise RuntimeError("❌ OPENAI_API_KEY not set in environment.")

# Parameters
N = 2  # Number of parallel completions
prompt = "A wise robot met a lost cat on Mars."
temperatures = [0.0, 1.0]
queues = [Queue() for _ in range(N)]
active = [True] * N
last_tokens = {}

# Selector function — replace with logic/Prolog later
def call_selector(token_dict):
    """Randomly selects one of the available stream IDs."""
    return random.choice(list(token_dict.keys()))

# Worker thread: stream OpenAI completions and push tokens into a queue
def run_stream(prompt, temperature, stream_id, queue):
    try:
        stream = openai.chat.completions.create(
            model="gpt-4o",
            messages=[{"role": "user", "content": prompt}],
            temperature=temperature,
            stream=True
        )
        for chunk in stream:
            # Fixed access for delta.content
            delta = chunk.choices[0].delta
            token = delta.content or ""
            if token:
                queue.put((stream_id, token))
    finally:
        queue.put((stream_id, None))  # Signal end of stream

# Launch all streams
threads = [
    Thread(target=run_stream, args=(prompt, temperatures[i], i, queues[i]))
    for i in range(N)
]
for t in threads:
    t.start()

# Main reasoning and output loop
while any(active):
    for i, q in enumerate(queues):
        if active[i] and i not in last_tokens:
            try:
                stream_id, token = q.get(timeout=0.2)
                if token is None:
                    active[i] = False
                else:
                    last_tokens[stream_id] = token
            except:
                pass  # Timeout — nothing to read

    if last_tokens:
        selected_id = call_selector(last_tokens)
        token = last_tokens.pop(selected_id)
        print(f"[{selected_id}] {token}", end="", flush=True)

# Cleanup
for t in threads:
    t.join()
