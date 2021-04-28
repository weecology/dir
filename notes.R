we want create to be the function and it work on the class
s3 methods dispatch

list1 <- list(main  = "main", 
              other = "other")
list2 <- list(main  = list(data    = "data", 
                           code    = "code",
                           results = "results"), 
              other = "other")
list3 <- list(main  = list(data    = "data", 
                           code    = "code",
                           results = list("2021-01-01" = "2021-01-01",
                                          "2021-01-02" = "2021-01-02")), 
              other = "other")

list_depth(list1)
list_depth(list2)
list_depth(list3)

articulate(list1)
articulate(list2)
articulate(list3)

x<-list3

depth<- function(x){
match.call()
}

depth(x)


file.path("main", c("poop", "oop"))