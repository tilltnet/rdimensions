ufl_names <- paste("site:ufl.edu", c("Jesse Dallery", "Andrew M O'Shea", "Walter Lana Leite", "Michael C Nechyba", "Sondra Smith-Adcock", "Joseph B Wiggins", "Jeff Boissoneault", "Richard A Griggs", "Anne Corinne Huggins-Manley", "Ronald A Cohen", "Daniela Seabra Oliveira", "Aishat Aloba", "Roland M Staud", "Janice L Raup Krieger", "Natalie Christina Ebner"))

library(searcher)
library(purrr)

map(
  ufl_names,
  search_google,
  rlang = FALSE
)


ufl_names2 <- paste("site:ufl.edu",c("Zizhao Zhang", "Yiannis G Ampatzidis", "Sabine Grunwald", "Roger Benton Fillingim", "Yadunandana N Rao", "Puskal P Pokharel", "Arunava Banerjee", "Yunmei Chen", "William R Hogan", "Chimay J Anumba", "Eakta Jain", "Juan E Gilbert", "Damon L Woodard", "Todd Matthew Manini", "Tezcan Ozrazgat-Baslanti", "Stephanie Ann Bohlman", "Renato J O Figueiredo", "Anis Davoudi", "Pierluigi Bortignon", "Yang Hu", "Joseph Wilson", "Carl D Crane", "Amr H Abd-Elrahman", "Mark S Schmalz", "Andreas Keil", "Kristy Elizabeth Boyer", "Jonathan C L Liu", "Lily-Ageliki Elefteriadou", "Alin Dobra", "Angelos Barmpoutis", "Eunju Kim", "Andrei P Kirilenko", "Kan Li", "Jian-Wu Xu", "Ahmed Helmy", "Arturo Suman Bretas", "Fujun Liu", "Patrick James Tighe"))

map(
  ufl_names2,
  ~{
    search_google(.,
  rlang = FALSE)
    Sys.sleep(6)

    }
)
