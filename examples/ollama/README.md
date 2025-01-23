# LispE Ollama Chat System

## Overview
This project implements a chat system using LispE, a Lisp interpreter, with Ollama for language model interactions.

## Files
- `prompting.lisp`: Main script for problem-solving and chat interaction
- `ollama.lisp`: Handles Ollama API connections via cURL
- `formatting.lisp`: Provides text formatting utilities

## Prerequisites
- LispE interpreter
- Ollama installed
- cURL library (comes with the interpreter itself)

## Configuration with `setprofile()`
### Function Signature
```lisp
(setprofile uri model max_tokens temperature)
```

### Parameters
- `uri`: Server address (default: "http://localhost:11434")
- `model`: Language model name (default: "mistral-small")
- `max_tokens`: Maximum response length (default: 1000)
- `temperature`: Response creativity (default: 0.7)

### Usage Examples
```lisp
; Change only URI
(setprofile "http://localhost:9000")

; Change URI and model
(setprofile "http://localhost:9000" "llama2")

; Change all parameters
(setprofile "http://localhost:9000" "llama2" 2000 0.5)
```

## Chat Function `tchat()`
### Function Signature
```lisp
(tchat messages prompt (system ""))
```

### Parameters
- `messages`: List of previous messages (empty list for first call)
- `prompt`: Current user message
- `system`: Optional system prompt (default: empty string)

### Example
```lisp
; First interaction with system prompt
(setq system "You are a helpful math tutor")
(setq msg (tchat () "What is 7 times 8?" system))

; Continue conversation
(setq next_msg (tchat msg "Can you explain how you solved that?"))
```

### Notes
- Omitted arguments retain previous values
- Passing `nil` keeps existing value

## Usage
1. Set system prompt and problem statement in `prompting.lisp`
2. Run the script to generate a response

### Example
```lisp
(setq problem `Sara and Joe have a combined height of 120 inches...`)
(setq msg (tchat () (replace prompt "{context}" problem) system))
(formatcontent (@ msg -1 "content"))
```

## Functions
- `setprofile()`: Configure server and model parameters
- `tchat()`: Initiate chat interaction
- `formatcontent()`: Apply text formatting

## Notes
- Requires external cURL library
- Designed for problem-solving and chat interactions
- Maintains conversation context across multiple calls
- System prompt is optional
- Requires external cURL library
