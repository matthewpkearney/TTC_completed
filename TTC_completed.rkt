; Author: Matthew Kearney
; Date:   May 2022
; Task:   Tiling the Courtyard
;  Ask user for input of any natural number (0,1,2,...)
;  Build a courtyard (I used 2 nested vectors)
;  One single tile (placed at 0,0 [check line 98]) will always be essential for the inductive proof, so based on n, 
;  we place trominos with our algorithm function so the trominos and the single tile fill the grid perfectly

;"I pledge my honor that I have abided by the Stevens Honor System."-MK
#lang racket
(define tiles 0) ; fill with this to start
(define (incr)#|increment at each new tromino |# (set! tiles (add1 tiles)) tiles); use set for manipulating already defined variables

;;prompt the program to start input automatically on compile-time
;keep input stored as n
(display "Please input the size of the courtyard tiles (n) you would like to use to create a courtyard of 2^n x 2^n:")
(define n (string->number (read-line (current-input-port) 'any))); sets input to variable n -> input size
(newline)
(define kxk (expt 2 n))


;making sizing first
(define crtyrd (make-vector kxk))
(for ([i (in-range 0 kxk)])
 (vector-set! crtyrd i (make-vector kxk tiles))); set the elements of the outside vector as vectors outside vecto

(define (printCourtyard)
(for ([i kxk])
  (println (vector-ref crtyrd i)))) ;call this after the algorithm has been implemented

;returns value at pos x, y
(define (getCrtyrd x y) 
  (vector-ref (vector-ref crtyrd y)  x ))
;setCourtyard
;sets the (x,y) coordinate of the vextor to value 'val'
(define (setCrtyrd x y val)
   (vector-set! (vector-ref crtyrd y)  x  val))
 
(define (place xi yi xj yj xk yk)
  ;(print (list xi yi xj yj xk yk)) ;debugging
  (incr)
  (setCrtyrd xi yi tiles)
  (setCrtyrd xj yj tiles)
  (setCrtyrd xk yk tiles))

(define (algorithm x y z)
  (define r 0);horizontal
  (define c 0);vertical
;when n == 2
(define (base_case)
  (incr) 
  (for ([i z])
    (for ([j z])
      (when (zero? (getCrtyrd (+ x i) (+ y j))) (setCrtyrd (+ x i) (+ y j) tiles)))))
  (cond
    [(= z 2)(base_case)] ;test 2x2s for where to place tromino
    [else 

     ;find rows and columns for next placement
   (for ([i (in-range x (+ x z))])
    (for ([j (in-range y (+ y z))]) 
      (when
          (not (zero? (getCrtyrd i j))) (and (set! r i) (set! c j)))))
        (cond
          [(and (< r (+ x  (/ z 2))) (< c (+ y (/ z 2))))   
          (place (+ x  (/ z 2))       (- (+ y (/ z 2)) 1)
                 (+ x  (/ z 2))      (+ y  (/ z 2))
                 (- (+ x  (/ z 2)) 1) (+ y  (/ z 2)))] ;br tro          
          [(and (>= r (+ x (/ z 2))) (< c (+ y  (/ z 2))))  
        (place (- (+ x  (/ z 2)) 1)  (+ y  (/ z 2))
                (+ x (/ z 2))        (+ y (/ z 2))
               (- (+ x  (/ z 2)) 1)  (- (+ y  (/ z 2)) 1) )] ;bl trom        
         [(and (< r (+ x (/ z 2))) (>= c ( + y  (/ z 2))))        
       (place (+ x  (/ z 2))       (- (+ y  (/ z 2)) 1)
               (+ x  (/ z 2))       (+ y (/ z 2))
               (- (+ x  (/ z 2)) 1)  (- (+ y  (/ z 2)) 1)  )] ;tr trom
          [(and (>= r (+ x  (/ z 2)))` (>= c (+ y  (/ z 2)))) 
              (place (- (+ x  (/ z 2)) 1)  (+ y (/ z 2))
              (+ x  (/ z 2))      (- (+ y  (/ z 2)) 1)
        (- (+ x (/ z 2)) 1)  (- (+ y  (/ z 2)) 1) )]) ;tl trom
        ;divide and conquer recursion
    (algorithm x (+ y (/ z 2)) (/ z 2)) 
    (algorithm x y   (/ z 2))
    (algorithm  (+ x (/ z 2)) y (/ z 2))
    (algorithm  (+ x (/ z 2))  (+ y (/ z 2)) (/ z 2))]))

;test the edge case n=0, else perform algorithm on n>=1
(cond
  [(= n 0)(printCourtyard)] ;edge case, n=0
  [else (and 
(newline)
(setCrtyrd (random kxk) (random kxk) -1);; for bottom right use ( - len 1), for top left use 0 
(algorithm 0 0 kxk) ;size the algorithm and start it at 0,0
(newline)
(newline)
(newline)
(printCourtyard))]) 
; once algorithm works (and (setCrtyrd 0 0 -1) (algorithm 0 0 kxk) (printCourtyard))

#|
Input format:
display message with the number you will operate the algorithm on


Output format:
Using vector(vectors), the program modifies a tromino at a time using the place function.
From here the algorithm knows where to place the next tromino
For n = 0, the program will return empty tile '0':
'#(0)

For n>=1, program will set a random tile location using (random kxk [length]) for the empty and
compute the algorithm from there. A few examples are provided below:
n=1
'#(-1 1)
'#(1 1)

n=2
'#(3 3 4 4)
'#(3 1 1 4)
'#(2 2 1 5)
'#(-1 2 5 5)

n=3
'#(9 9 10 10 14 14 15 15)
'#(9 7 7 10 14 12 12 15)
'#(8 7 11 11 13 13 12 16)
'#(8 8 11 1 1 13 16 16)
'#(4 4 5 5 1 19 20 20)
'#(4 2 2 5 19 19 17 20)
'#(3 2 6 6 18 17 17 21)
'#(3 3 6 -1 18 18 21 21)

n=5, the output will begin to look less and less like a courtyard though the algorithm still preforms properly
'#(117 117 118 118 122 122 123 123 138 138 139 139 143 143 144 144 202 202 203 203 207 207 208 208 223 223 224 224 228 228 229 229)
'#(117 115 115 118 122 120 120 123 138 136 136 139 143 141 141 144 202 200 200 203 207 205 205 208 223 221 221 224 228 226 226 229)
'#(116 115 119 119 121 121 120 124 137 136 140 140 142 142 141 145 201 200 204 204 206 206 205 209 222 221 225 225 227 227 226 230)
'#(116 116 119 109 109 121 124 124 137 137 140 130 130 142 145 145 201 201 204 194 194 206 209 209 222 222 225 215 215 227 230 230)
'#(112 112 113 109 127 127 128 128 133 133 134 134 130 148 149 149 197 197 198 194 212 212 213 213 218 218 219 219 215 233 234 234)
'#(112 110 113 113 127 125 125 128 133 131 131 134 148 148 146 149 197 195 198 198 212 210 210 213 218 216 216 219 233 233 231 234)
'#(111 110 110 114 126 125 129 129 132 132 131 135 147 146 146 150 196 195 195 199 211 210 214 214 217 217 216 220 232 231 231 235)
'#(111 111 114 114 126 126 129 87 87 132 135 135 147 147 150 150 196 196 199 199 211 211 214 172 172 217 220 220 232 232 235 235)
'#(96 96 97 97 101 101 102 87 159 159 160 160 164 164 165 165 181 181 182 182 186 186 187 187 172 244 245 245 249 249 250 250)
'#(96 94 94 97 101 99 102 102 159 157 157 160 164 162 162 165 181 179 179 182 186 184 184 187 244 244 242 245 249 247 247 250)
'#(95 94 98 98 100 99 99 103 158 157 161 161 163 163 162 166 180 179 183 183 185 185 184 188 243 242 242 246 248 248 247 251)
'#(95 95 98 88 100 100 103 103 158 158 161 151 151 163 166 166 180 180 183 173 173 185 188 188 243 243 246 246 236 248 251 251)
'#(91 91 92 88 88 106 107 107 154 154 155 155 151 169 170 170 176 176 177 177 173 191 192 192 239 239 240 236 236 254 255 255)
'#(91 89 92 92 106 106 104 107 154 152 152 155 169 169 167 170 176 174 174 177 191 191 189 192 239 237 240 240 254 254 252 255)
'#(90 89 89 93 105 104 104 108 153 153 152 156 168 167 167 171 175 175 174 178 190 189 189 193 238 237 237 241 253 252 252 256)
'#(90 90 93 93 105 105 108 108 153 -1 156 156 168 168 171 171 1 175 178 178 190 190 193 193 238 238 241 241 253 253 256 256)
'#(32 32 33 33 37 37 38 38 53 53 54 54 58 58 59 1 1 287 288 288 292 292 293 293 308 308 309 309 313 313 314 314)
'#(32 30 30 33 37 35 35 38 53 51 51 54 58 56 59 59 287 287 285 288 292 290 290 293 308 306 306 309 313 311 311 314)
'#(31 30 34 34 36 36 35 39 52 51 55 55 57 56 56 60 286 285 285 289 291 291 290 294 307 306 310 310 312 312 311 315)
'#(31 31 34 24 24 36 39 39 52 52 55 45 57 57 60 60 286 286 289 289 279 291 294 294 307 307 310 300 300 312 315 315)
'#(27 27 28 24 42 42 43 43 48 48 49 45 45 63 64 64 282 282 283 279 279 297 298 298 303 303 304 304 300 318 319 319)
'#(27 25 28 28 42 40 40 43 48 46 49 49 63 63 61 64 282 280 283 283 297 297 295 298 303 301 301 304 318 318 316 319)
'#(26 25 25 29 41 40 44 44 47 46 46 50 62 61 61 65 281 280 280 284 296 295 295 299 302 302 301 305 317 316 316 320)
'#(26 26 29 29 41 41 44 2 47 47 50 50 62 62 65 65 281 281 284 284 296 296 299 299 257 302 305 305 317 317 320 320)
'#(11 11 12 12 16 16 17 2 2 74 75 75 79 79 80 80 266 266 267 267 271 271 272 257 257 329 330 330 334 334 335 335)
'#(11 9 9 12 16 14 17 17 74 74 72 75 79 77 77 80 266 264 264 267 271 269 272 272 329 329 327 330 334 332 332 335)
'#(10 9 13 13 15 14 14 18 73 72 72 76 78 78 77 81 265 264 268 268 270 269 269 273 328 327 327 331 333 333 332 336)
'#(10 10 13 3 15 15 18 18 73 73 76 76 66 78 81 81 265 265 268 258 270 270 273 273 328 328 331 331 321 333 336 336)
'#(6 6 7 3 3 21 22 22 69 69 70 66 66 84 85 85 261 261 262 258 258 276 277 277 324 324 325 321 321 339 340 340)
'#(6 4 7 7 21 21 19 22 69 67 70 70 84 84 82 85 261 259 262 262 276 276 274 277 324 322 325 325 339 339 337 340)
'#(5 4 4 8 20 19 19 23 68 67 67 71 83 82 82 86 260 259 259 263 275 274 274 278 323 322 322 326 338 337 337 341)
'#(5 5 8 8 20 20 23 23 68 68 71 71 83 83 86 86 260 260 263 263 275 275 278 278 323 323 326 326 338 338 341 341)
Since I use a number system and not a graphics library, I don't have it working on one page for examples n>=6 as it goes off the page,
although n = 7 works in less than a few seconds (on my machine)
I start the timer once I press enter on the n I would like the algorithm to perform on
|#
