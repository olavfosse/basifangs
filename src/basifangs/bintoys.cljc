(ns basifangs.bintoys
  "For now the bytes representation is vector of ints from 0-255. Might
  try clope later."
  (:require [basifangs.utils :as bfu]
            [no.olavfosse.xio :as xio]))



(defn partition-bytes [partitions bs]
  (into {}
        (comp (partition-all 2)
              (map (let [!bs (atom bs)]
                     (fn [[field width]]
                       (let [v (into [] (take width) @!bs)]
                         (swap! !bs (partial drop width))
                         [field v])))))
        partitions))

(defn elf64:endianness [elf]
  ;; We can't have \xAB escape sequences in cljc files, because
  ;; Clojure's reader throws an exception, even if they're
  ;; behind :lpy reader conditionals
  ;;
  ;; Broken unforunately:
  ;; (case (get-in elf [:header/e_ident :EI_DATA])
  ;;   #b"\x01" :little
  ;;   #b"\x02" :big)
  (case (get-in elf [:header/e_ident :EI_DATA 0])
    1 :little
    2 :big))

(defn tee [x] (prn x) x)



(defn elf64:parse-header [bs] 
  ;; typedef struct elf64_hdr {
  ;; 	unsigned char e_ident[16];	/* ELF "magic number" */
  ;; 	Elf64_Half e_type;
  ;; 	Elf64_Half e_machine;
  ;; 	Elf64_Word e_version;
  ;; 	Elf64_Addr e_entry;	/* Entry point virtual address */
  ;; 	Elf64_Off e_phoff;	/* Program header table file offset */
  ;; 	Elf64_Off e_shoff;	/* Section header table file offset */
  ;; 	Elf64_Word e_flags;
  ;; 	Elf64_Half e_ehsize;
  ;; 	Elf64_Half e_phentsize;
  ;; 	Elf64_Half e_phnum;
  ;; 	Elf64_Half e_shentsize;
  ;; 	Elf64_Half e_shnum;
  ;; 	Elf64_Half e_shstrndx;
  ;; } Elf64_Ehdr;

  ;; NB: I only parsed the fields necesary to parse the program header
  ;;     table. I'm honestly more interested in generating programs
  ;;     than parsing them.
  ;;
  ;; TODO(next): parse program header table (see wikipedia)
  ;; TODO: produce a minimal program
  (let [elf (partition-bytes [:header/e_ident 16
                              :header/e_type 2
                              :header/e_machine 2
                              :header/e_version 4
                              :header/e_entry 8
                              :header/e_phoff 8
                              :header/e_shoff 8
                              :header/e_flags 4
                              :header/e_ehsize 2
                              :header/e_phentsize 2
                              :header/e_phnum 2
                              :header/e_shentsize 2
                              :header/e_shnum 2
                              :header/e_shstrndx 2]
                             bs)
        elf (update elf :header/e_ident (partial partition-bytes
                                                 [:EI_MAG0 1
                                                  :EI_MAG1 1
                                                  :EI_MAG2 1
                                                  :EI_MAG3 1
                                                  :EI_CLASS 1
                                                  :EI_DATA 1
                                                  :EI_VERSION 1
                                                  :EI_OSABI 1
                                                  :EI_ABIVERSION 1
                                                  :EI_PAD 7]))
        elf (update elf :header/e_phoff (partial bfu/bytes->int (elf64:endianness elf)))]
    elf))


(defn elf64:parse-pht [bs])

(defn elf64:parse [bs]
  (let [header (elf64:parse-header (into [] (take 64) bs))
        pht (->> bs (drop (:header/e_phoff header)) elf64:parse-pht)]
    (merge header pht)))

(comment
  (elf64:endianness (elf64:parse (xio/bslurp "/Users/olav/Hacking/basifangs/workspace/hello.elf")))
  (elf64:parse (xio/bslurp "/Users/olav/Hacking/basifangs/workspace/hello.elf"))
  (bfu/bytes->hex (mapv #(- % 128)  (xio/bslurp "/Users/olav/Hacking/basifangs/workspace/hello.elf")))

  (count (xio/bslurp-raw "/Users/olav/Hacking/basifangs/workspace/hello.elf") )27192
  ;; $ wc -c /Users/olav/Hacking/basifangs/workspace/hello.elf 27192
  )
