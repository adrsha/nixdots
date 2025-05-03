(use-package ellama
  :ensure t
  ;; Binding to open the main transient menu
  :bind ("C-c e" . ellama-transient-main-menu)
  
  :init
  ;; Language setting for translations and interactions
  (setopt ellama-language "English")
  
  ;; Require the Ollama LLM package
  (require 'llm-ollama)
  
  ;; Main provider configuration for Gemma 3 27B
  (setopt ellama-provider
          (make-llm-ollama
           ;; Specify the Gemma 3 27B model
           :chat-model "gemma3:27b"
           ;; Use a suitable embedding model
           :embedding-model "nomic-embed-text"
           ;; Set context window size
           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  
  ;; Summarization provider - best options
  (setopt ellama-summarization-provider
          (make-llm-ollama
           ;; Mixtral is excellent for nuanced summarization
           :chat-model "mixtral:8x7b-instruct-v0.1-q4_K_M"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  
  ;; Coding provider - best options
  (setopt ellama-coding-provider
          (make-llm-ollama
           ;; DeepSeek Coder is top-tier for coding tasks
           :chat-model "deepseek-coder:33b-instruct-q4_K_M"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  
  ;; Optional: Additional providers for interactive switching
  (setopt ellama-providers
          '(("gemma" . (make-llm-ollama
                        :chat-model "gemma3:27b"
                        :embedding-model "nomic-embed-text"))
            ("mixtral" . (make-llm-ollama
                          :chat-model "mixtral:8x7b-instruct-v0.1-q4_K_M"
                          :embedding-model "nomic-embed-text"))
            ("deepseek-coder" . (make-llm-ollama
                                 :chat-model "deepseek-coder:33b-instruct-q4_K_M"
                                 :embedding-model "nomic-embed-text"))))
  
  ;; Naming provider for new sessions
  (setopt ellama-naming-provider
          (make-llm-ollama
           :chat-model "gemma3:27b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("stop" . ("\n")))))
  
  ;; Use LLM for generating session names
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  
  ;; Translation provider
  (setopt ellama-translation-provider
          (make-llm-ollama
           :chat-model "gemma3:27b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params
           '(("num_ctx" . 32768))))
  
  ;; Extraction provider
  (setopt ellama-extraction-provider 
          (make-llm-ollama
           :chat-model "gemma3:27b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params
           '(("num_ctx" . 32768))))
  
  ;; Customize buffer display behavior
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  
  :config
  ;; Show Ellama context in header line globally
  (ellama-context-header-line-global-mode +1)
  
  ;; Handle scrolling events
  (advice-add 'pixel-scroll-precision :before #'ellama-disable-scroll)
  (advice-add 'end-of-buffer :after #'ellama-enable-scroll))
