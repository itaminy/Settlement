(ns settlement.framing
  (:import [java.io BufferedInputStream ByteArrayOutputStream IOException]
           [java.nio ByteBuffer]
           [java.nio.charset StandardCharsets]))

(def max-request-bytes (* 8 1024))

(defn read-line!
  "Reads one UTF-8 request bounded by LF or CRLF. Returns nil at EOF."
  [^BufferedInputStream input]
  (let [buffer (ByteArrayOutputStream.)]
    (loop []
      (let [byte (.read input)]
        (case byte
          -1 nil
          10 (let [bytes (.toByteArray buffer)
                   size (alength bytes)
                   length (if (and (pos? size) (= 13 (aget bytes (dec size))))
                            (dec size)
                            size)]
               (str (.decode (.newDecoder StandardCharsets/UTF_8)
                             (ByteBuffer/wrap bytes 0 length))))
          (do
            (when (>= (.size buffer) max-request-bytes)
              (throw (IOException. "Request exceeds byte limit")))
            (.write buffer byte)
            (recur)))))))
