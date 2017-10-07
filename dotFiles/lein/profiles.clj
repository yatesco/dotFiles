{:user {:plugins [[lein-ancient "0.6.10"]
                  [lein-pprint "1.1.2"]
                  [com.jakemccrary/lein-test-refresh "0.16.0"]]}
     :env {:squiggly {:checkers [:eastwood]
                      :eastwood-exclude-linters [:unlimited-use]}}}

