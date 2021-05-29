submit_mp3.transcription <- function(...) {
  # ... = file paths (various number of files allowed)
  # Returns file names as shown in S3
  
  files <- list(...)
  
  commands <- unlist(files) %>% 
    paste('aws s3 cp raw_data/', ., ' s3://sang.math110', sep = '')
  
  for(i in commands) system(i)
  
  return(unlist(files))
}

start_transcibing.transcription <- function(...) {
  # ... = mp3 files stored on S3
  # Returns a vector of job names (equal to number of input files)
  
  file_names <- list(...)
  jobs <- vector(mode = 'character')
  
  # Create json files and transcribe
  for(i in unlist(file_names)) {
    job_name <- str_glue('math110', runif(1))
    jobs <- append(jobs, job_name)
    
    # Prepare json file
    out <- list(
      TranscriptionJobName = job_name,
      LanguageCode = 'en-US',
      MediaFormat = 'mp3',
      Media = list(MediaFileUri = str_glue('https://sang.math110.s3.amazonaws.com/', i)),
      OutputBucketName = 'sang.math110.transcribed'
    )
    
    # Write json file
    write_json(out, str_glue('json_files/', job_name, '.json'), auto_unbox = TRUE)
    
    # System call
    str_glue('aws transcribe start-transcription-job --region us-east-1 --cli-input-json file://json_files/',
             job_name, '.json') %>%
      system
  }
  
  return(jobs)
}

check_status.transcription <- function(...) {
  # ... = job names to check
  # Returns a logical vector with length = number of input jobs 
  # (TRUE = completed, FALSE = in progress or otherwise)
  
  out <- vector(mode = 'logical')
  jobs <- list(...)
  
  for(i in unlist(jobs)) {
    # 1. Identify the job & write its status to a txt file
    paste('aws transcribe list-transcription-jobs --region us-east-1 --job-name-contains',
          i, '> dynamic/status.txt') %>% 
      system
    status <- read_json('dynamic/status.txt', simplifyVector = TRUE)
    
    # 2. Check if status is empty
    if(is_empty(status$TranscriptionJobSummaries$TranscriptionJobStatus)) {
      paste('aws transcribe list-transcription-jobs --region us-east-1 --job-name-contains',
               i, '--next-token', status$NextToken,'> dynamic/status.txt') %>% 
        system
      status <- read_json('dynamic/status.txt', simplifyVector = TRUE)
    }
    
    # 3. Save status to out
    if(status$TranscriptionJobSummaries$TranscriptionJobStatus == 'COMPLETED') 
      out <- append(out, TRUE) 
    else out <- append(out, FALSE)
  }
  
  return(out)
}

get_transcript.transcription <- function(...) {
  # ... = job names
  # Returns a character vector of transcripts
  
  out <- vector(mode = 'character')
  jobs <- list(...)
  
  for(i in unlist(jobs)) {
    
    # Download transcript
    str_glue('aws s3 cp s3://sang.math110.transcribed/',
             i ,'.json dynamic/transcription_output.json') %>%
      system
    
    # Save transcript to out
    j <- read_json('dynamic/transcription_output.json', simplifyVector = TRUE)
    out <- j$results$transcripts$transcript %>%
      str_remove('\'|\"') %>%
      append(out, .)
  }
  
  return(out)
}

write_transcripts.transcription <- function(names, ...) {
  # names = names of text files
  # ... = transcripts (strings)
  # Returns a vector of paths to transcripts
  
  transcripts = list(...)
  i <- 1
  
  for(transcript in unlist(transcripts)) {
    writeLines(transcript, str_glue('transcripts/', names[i], '.txt'))
    i = i + 1
  }
  
  return(list.files('transcripts', full.names = TRUE))
}

translate <- function(...) {
  # ... = paths to original texts
  # Returns a character vector of translated texts
  
  files <- list(...)
  transcripts <- map_chr(unlist(files), function(x) readChar(x, nchars = 5000, useBytes = TRUE))
    # Read in at max 5000 bytes
  out <- vector(mode = 'character')
  
  for(transcript in transcripts) {
    # Translate
    Sys.setenv(text = transcript)
    system('aws translate translate-text --text \"$text\" --source-language-code en --target-language-code fr > dynamic/french.txt')
    
    # Save translation to out
    j <- read_json('dynamic/french.txt', simplifyVector = TRUE)
    out <- j$TranslatedText %>%
      str_remove('\'|"') %>%
      append(out, .)
  }
  
  return(out)
}

write_translations <- function(names, ...) {
  # names = names of translated text files
  # ... = translations (strings)
  # Returns a vector of paths to French translations
  
  translations = list(...)
  i <- 1
  
  for(translation in unlist(translations)) {
    writeLines(translation, str_glue('translated_texts/', names[i], '.txt'))
    i = i + 1
  }
  
  return(list.files('translated_texts', full.names = TRUE))
}

speak <- function(names, ...) {
  # names = names of mp3 files
  # ... = text files
  # Returns a vector of paths to mp3 files
  
  textfiles <- list(...)
  i <- 1
  
  for(textfile in unlist(textfiles)) {
    
    # Convert text -> audio and store the audio file in folder translated_speeches
    Sys.setenv(text = readChar(textfile, nchar = 3000))
    str_glue('aws polly synthesize-speech --output-format mp3 --voice-id Mathieu --text \"$text\" translated_speeches/',
          names[i], '.mp3') %>%
      system
    
    i = i + 1
  }
  
  return(list.files('translated_speeches', full.names = TRUE))
}