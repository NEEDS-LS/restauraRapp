############################################################################################ Splitting multipart polygons and attributing holes

gExplode <- function(to.exp) {
    
    length.to.exp <- length(to.exp)
    names.list.to.exp <- NULL
    pol.to.exp <- list()
    all <- data.frame()
    
    for (i in 1:length.to.exp) {
        objets.to.exp.ob <- sapply(slot(to.exp, "polygons")[i], slot, "Polygons")
        objets.to.exp.ID <- sapply(slot(to.exp, "polygons")[i], slot, "ID")
        
        for (j in 1:length(objets.to.exp.ob)) {
            hole <- slot(objets.to.exp.ob[[j]], "hole")
            pol1.to.exp <- objets.to.exp.ob[[j]]
            pol2.to.exp <- Polygons(list(pol1.to.exp), c(paste(objets.to.exp.ID, j, sep = ":")))
            names.list.to.exp <- c(names.list.to.exp, c(paste(paste(objets.to.exp.ID, j, sep = ":"), 
                hole, sep = ";")))
            pol4.to.exp <- SpatialPolygons(list(pol2.to.exp), proj4string = CRS("+proj=utm +zone=24 +south +ellps=aust_SA +units=m +no_defs"))
            pol.to.exp <- c(slot(pol4.to.exp, "polygons"), pol.to.exp)
        }
    }
    
    names.list.to.exp.df <- data.frame(names.list.to.exp)
    
    names.list.to.exp.df$hole <- lapply(strsplit(as.character(names.list.to.exp.df$names.list.to.exp), 
        "\\;"), "[", 2)
    
    names.list.to.exp.df$id <- lapply(strsplit(as.character(names.list.to.exp.df$names.list.to.exp), 
        "\\;"), "[", 1)
    
    row.names(names.list.to.exp.df) <- as.character(names.list.to.exp.df$id)
    
    s <- rep("stab", dim(names.list.to.exp.df)[1])
    
    names.list.to.exp.df <- cbind(names.list.to.exp.df, s)
    
    Fragments.to.exp <- SpatialPolygons(pol.to.exp, pO = 1:length(pol.to.exp), proj4string = CRS("+proj=utm +zone=24 +south +ellps=aust_SA +units=m +no_defs"))
    
    Fragments.to.exp.df <- SpatialPolygonsDataFrame(Fragments.to.exp, names.list.to.exp.df)
    
    Frags.to.exp.no.hole <- Fragments.to.exp.df[Fragments.to.exp.df$hole == FALSE, ]
    
    if (dim(Frags.to.exp.no.hole)[1] == dim(Fragments.to.exp.df)[1]) 
        Frag <- Frags.to.exp.no.hole
    
    if (dim(Frags.to.exp.no.hole)[1] != dim(Fragments.to.exp.df)[1]) {
        Frags.to.exp.hole <- Fragments.to.exp.df[Fragments.to.exp.df$hole == TRUE, ]
        Frags.no.holes <- Frags.to.exp.no.hole
        Frags.holes <- Frags.to.exp.hole
        Frags.holes <- gBuffer(Frags.holes, byid = TRUE, width = 0)  #This solves the problem of Self-intersection rings
        Frag <- gHoles(Frags.no.holes, Frags.holes)
    }
    return(Frag)
}
###########################################################################################  
