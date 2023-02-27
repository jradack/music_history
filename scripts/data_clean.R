################################################################################ 
# Name: music_history/scripts/00-data_clean.R
# Created by: Josh Radack
# Date: 2022-12-19
# Purpose: Cleans last.fm user data, incorporates genre information from
#          musicbrainz.
################################################################################

packs <- c("dplyr","lubridate","httr","jsonlite","ratelimitr")
lapply(packs, library, character.only = TRUE)

# Load data
scrobbles_raw <- read.csv("data/raw/scrobbles-john_apple_seed-1671206829.csv")

# Clean variables and filter to valid years of data
scrobbles <- scrobbles_raw |> 
  mutate(date = as_date(substr(utc_time, 1, 11), format = "%d %b %Y"),
         year = year(date),
         time = hm(substr(utc_time, 14, 18))) |> 
  filter(year >= 2011)

# Get list of artists and artist ID
artists <- scrobbles |> 
  distinct(artist) |> 
  left_join(scrobbles |> distinct(artist, artist_mbid) |> filter(artist_mbid != ""), by = "artist")

artist_album <- scrobbles |> 
  distinct(artist, album) |> 
  inner_join(scrobbles |> 
              distinct(artist, artist_mbid, album, album_mbid) |> 
              filter(artist_mbid == "" & album_mbid != ""), 
            by = c("artist", "album"))


# Query all requisite artist information using musicbrainz package
root <- "https://musicbrainz.org/ws/2/"

# When album ID is available, query album to get artist ID
# Function for API request of albums based on album ID, returns a dataframe
# containing album information and artist ID
queryAlbum <- function(album_id){
  url <- paste0(root, "release/", album_id, "?inc=artist-credits&fmt=json")
  res <- GET(url)
  dat <- fromJSON(rawToChar(res$content))$`artist-credit`$artist
  
  return(cbind(album_id = rep(album_id, nrow(dat)), dat))
}
queryAlbum_lim <- limit_rate(queryAlbum, rate(n = 1, period = 1))

album_id <- "7e0867e7-9d32-4fa7-820b-74234c7594f3" # Frailty, Jane Removers
queryAlbum_lim(album_id)

# Function for filling in artist ID based on the album ID
queryAlbumAll <- function(artist_album){
  n <- nrow(artist_album)
  multi_res <- data.frame()
  pb <- txtProgressBar(min = 0, max = n, initial = 0, style = 3) 
  for(i in 1:n){
    setTxtProgressBar(pb,i)
    # Make query to musicbrainz using album ID
    album_query <- queryAlbum_lim(artist_album$album_mbid[i])
    
    if(nrow(album_query) == 1){
      # If only one result is returned, assign to data frame
      artist_album$artist_mbid[i] <- album_query$id[1]
    } else {
      # Go through multiple artist results, match on name and sort name
      for(j in 1:nrow(album_query)){
        if(artist_album$artist[i] %in% album_query[j,c("name","sort-name")]){
          artist_album$artist_mbid[i] <- album_query$id[j]
          break
        }
      }
      # Save all results for future reference
      multi_res <- rbind(multi_res, album_query)
    }
  }
  close(pb)
  
  return(list(artist_album = artist_album, multi_res = multi_res))
}

queryAlbum_res <- queryAlbumAll(artist_album)
saveRDS(queryAlbum_res, "data/raw/album_query_20221220.RDS")

# Merge artist ID from the album query to master list
artists <- artists |> 
  left_join(queryAlbum_res[["artist_album"]] |> 
              filter(artist_mbid != "") |> 
              select(artist, artist_mbid) |> 
              distinct(),
            by = "artist") |>
  mutate(artist_mbid = coalesce(artist_mbid.x, artist_mbid.y)) |> 
  select(artist, artist_mbid)



# Search artist name to get fill in artist ID
searchArtistID <- function(artist_name){
  url <- paste0(root, "artist?query=", artist_name, "&limit=10&fmt=json")
  res <- GET(url)
  dat <- fromJSON(rawToChar(res$content))$artists[,c("id","score","name","sort-name")]
  return(dat)
}
searchArtistID_lim <- limit_rate(searchArtistID, rate(n = 1, period = 1))

artist_name <- "My Dead Girlfriend"
artist_search <- searchArtistID_lim(URLencode(artist_name))

searchArtistAll <- function(artists){
  n <- nrow(artists)
  # n <- 10
  multi_res <- data.frame()
  pb <- txtProgressBar(min = 0, max = n, initial = 0, style = 3) 
  for(i in 1:n){
    setTxtProgressBar(pb,i)
    
    # If artist ID already exists, skip to next
    if(!is.na(artists$artist_mbid[i]) & artists$artist_mbid[i] != ""){
      next
    }
    
    # Search musicbrainz for matching artists, keep only exact matches
    artist_str <- URLencode(artists$artist_mbid[i])
    artist_query <- searchArtistID_lim(artist_str) |> 
      filter(score == 100)
    
    if(nrow(artist_query) == 1){
      # If only one result is returned, assign to data frame
      artists$artist_mbid[i] <- artist_query$id[1]
    } else {
      # Save all results for future reference
      multi_res <- rbind(multi_res, artist_query)
    }
  }
  close(pb)
  
  return(list(artists = artists, multi_res = multi_res))
}

artist_search_res <- searchArtistAll(artists)
saveRDS(artist_search_res, "data/raw/artist_search_20221220.RDS")

# check how many artists still have missing ID
sum(is.na(artist_search_res$artists$artist_mbid) | artist_search_res$artists$artist_mbid == "")


