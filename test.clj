(ns myproject.core
  (:require [duckling.core :as p]))

(p/load! '(:languages ["en"]));; Load all languages

(p/parse :en$core ;; core configuration for English ; see also :fr$core, :es$core, :zh$core
         "wake me up the last Monday of January 2015 at 6am"
[:time]) ;; We are interested in :time expressions only ; see also :duration, :temperature, etc.
