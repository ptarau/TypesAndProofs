import openai
import os
import random
from threading import Thread
from queue import Queue
import clingo
import unicodedata

# Get API key from environment
openai.api_key = os.getenv("OPENAI_API_KEY")
if not openai.api_key:
    raise RuntimeError("❌ OPENAI_API_KEY not set in environment.")

# Parameters
N = 2  # Number of completions
prompt = "A wise robot met a lost cat on Mars."
temperatures = [0.0, 1.0]
queues = [Queue() for _ in range(N)]
active = [True] * N
last_tokens = {}
history = {i: [] for i in range(N)}

# Escape function for Clingo input
def escape(token: str) -> str:
    token = unicodedata.normalize("NFKD", token)
    token = token.replace('"', ' ')
    token = token.replace('\n', ' ')
    token = token.replace('\r', ' ')
    token = token.replace('\\', '\\\\')
    token = ''.join(c for c in token if c.isprintable())
    return token.strip()

# ASP-based stream selector
def call_selector_asp(token_dict, history_dict):
    ctl = clingo.Control()

    for stream_id, token in token_dict.items():
        safe_token = escape(token)
        ctl.add("base", [], f'token({stream_id}, "{safe_token}").')
        ctl.add("base", [], f'stream({stream_id}).')

    for stream_id, tokens in history_dict.items():
        for i, t in enumerate(tokens):
            safe_t = escape(t)
            ctl.add("base", [], f'history({stream_id}, {i}, "{safe_t}").')

    asp_logic = """
    length(S,L) :- token(S,T), L = string_length(T).

    less(L,S) :-
        length(S,L),
        length(S2,L2),
        S2 != S,
        L2 > L.

    best(S) :- length(S,L), not less(L,S).
    selected(S) :- best(S).

    #show selected/1.
    """
    ctl.add("base", [], asp_logic)

    try:
        ctl.ground([("base", [])])
    except RuntimeError as e:
        print("❌ ASP grounding error:", e)
        return None

    result = []
    def on_model(model):
        result.extend(model.symbols(shown=True))

    ctl.solve(on_model=on_model)

    for atom in result:
        if atom.name == "selected" and len(atom.arguments) == 1:
            return atom.arguments[0].number
    return None

# Stream thread worker
def run_stream(prompt, temperature, stream_id, queue):
    try:
        stream = openai.chat.completions.create(
            model="gpt-4o",
            messages=[{"role": "user", "content": prompt}],
            temperature=temperature,
            stream=True
        )
        for chunk in stream:
            delta = chunk.choices[0].delta
            token = delta.content or ""
            if token:
                queue.put((stream_id, token))
    finally:
        queue.put((stream_id, None))

# Launch streaming threads
threads = [
    Thread(target=run_stream, args=(prompt, temperatures[i], i, queues[i]))
    for i in range(N)
]
for t in threads:
    t.start()

# Main control loop
while any(active):
    for i, q in enumerate(queues):
        if active[i] and i not in last_tokens:
            try:
                stream_id, token = q.get(timeout=0.2)
                if token is None:
                    active[stream_id] = False
                else:
                    last_tokens[stream_id] = token
            except:
                pass

    if last_tokens:
        selected_id = call_selector_asp(last_tokens, history)
        if selected_id is not None and selected_id in last_tokens:
            token = last_tokens.pop(selected_id)
            print(f"[{selected_id}] {token}", end="", flush=True)
            history[selected_id].append(token)

# Wait for all threads
for t in threads:
    t.join()
