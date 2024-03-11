#lang racket

(define filter_option->index
  (hash "index" 0
        "rank" 1
        "name" 2
        "platform" 3
        "date" 4
        "genre" 5
        "publisher" 6
        "north_america" 7
        "europe" 8
        "japan" 9
        "rest_of_world" 10
        "global" 11
        "review" 12))

(define menu_options
  (list "name" "date" "publisher" "region" "genre"))

(define (enumerate lst)
  (for/list ([item lst] [index (in-naturals)])
    (cons index item)))

; Prompt user for game title
(define (input_game_name)
  (printf "Enter game name: ")
  (read))

; Prompt user for date range
(define (input_date_range)
  (printf "Enter start year: ")
  (let ((start (read)))
    (printf "Enter end year: ")
    (let ((end (read)))
      (if (> start end) (list end start) (list start end)))))

; Prompt user for publisher
(define (input_publisher_name)
  (printf "Enter publisher name: ")
  (read))

; Prompt user for region
(define (input_region)
  (printf "Choose region: (North America, Europe, Japan, Rest of World, Global) ")
  (read))

; Prompt user for genre
(define (input_genre)
  (printf "Enter game genre: ")
  (read))

; User input
(define (get_menu_input)
  (let loop ((input_selection '()))
    (cond
      [(= (length input_selection) 3) input_selection]
      [else
       (printf "Currently selected filter: ~a\n" (map (lambda (pair) (car pair)) input_selection))
       (printf "Select an option by the number attached to it:\n1.) Name\n2.) Date\n3.) Publisher\n4.) Region\n5.) Genre\nType 'done' when finished\n")
       (let ((selection (read)))
         (cond
           [(equal? selection 'done) input_selection]
           [(integer? selection)
            (case selection
              [(1) (loop (cons (cons 'name (input_game_name)) input_selection))]
              [(2) (loop (cons (cons 'date (input_date_range)) input_selection))]
              [(3) (loop (cons (cons 'publisher (input_publisher_name)) input_selection))]
              [(4) (loop (cons (cons 'region (input_region)) input_selection))]
              [(5) (loop (cons (cons 'genre (input_genre)) input_selection))]
              [else
               (printf "Invalid input.\n")
               (loop input_selection)])]
           [else
            (printf "Invalid input.\n")
            (loop input_selection)]))])))

; Split entries in csv by ','
(define (parse_csv filepath)
  (with-input-from-file filepath
    (lambda ()
      (let loop ((lines '()))
        (let ((line (read-line)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons (string-split (string-trim line) ",") lines))))))))

; Process raw data to be cleaned up in output_list function
(define (split_csv filepath selected-criteria)
  (let* ((wack_data (parse_csv filepath))
         (headers (first wack_data))
         (data (rest wack_data)))
    data))

; Return list of data that has had the filter applied
(define (output_list filepath filter_option)
  (let* ((cool_data (split_csv filepath filter_option)))
    cool_data))

(define (main)
  (let loop ()
    (printf "Please select up to three filters from the following options:\n")
    (let ((user_menu_input (get_menu_input)))
      (when (not (null? user_menu_input))
        (printf "You selected: ~a\n" (map (lambda (pair) (car pair)) user_menu_input))
        (let ((output_list (output_list "Video Games Sales.csv" user_menu_input)))
       
          (printf "Displaying the results based on your filters\n")
          (for-each (lambda (row) (displayln row)) (take output_list 5))
          (printf "Do you want to do another search?\n1.) Yes\n2.) No \n")
          (when (equal? (read) '1) (loop)))))))

(main)