# chat2doc 0.1.0.9000

## NEW FEATURES

* 텍스트 처리 함수 추가 및 도움말 작성. (#5).
    - 추가 함수
        - remove_before_string()
        - remove_before_first()
    - 도움말 작성 함수
        - trim_before_string()
        - extract_between()
        - remove_before_string()
        - remove_before_first()
        
## MINOR CHANGES

* extract_subtitles(), common_subtitle() 개선. (#6).


        
# chat2doc 0.0.4

## NEW FEATURES

* Support xAI Grok. (#4).
    - add functions
        - list_models_xai()
* Support authoring.
    - add functions
        - chat2markdown(). only manpage
        - cat_chat()   
        - extract_subtitles()
        
        
        
# chat2doc 0.0.3

## NEW FEATURES

* Support xAI Grok. (#3).
    - add functions
        - chat_grok()
        
## MINOR CHANGES

* Support xAI Grok. (#3).
    - change functions for Grok
        - regist_api_key()
        - set_api_key()        
        

        
# chat2doc 0.0.2

## MAJOR CHANGES

* API key 핸들링 함수의 통합. (#2).
    - remove functions
        - regist_openai_key()
        - regist_anthropic_key()
        - regist_google_key()
        - set_openai_key()
        - set_anthropic_key()
        - set_google_key()
    - add functions
        - regist_api_key()
        - set_api_key()



# chat2doc 0.0.1

## NEW FEATURES

* 채팅 결과의 요약 및 편집 기능 추가. (#1).
    - edit_openai()
