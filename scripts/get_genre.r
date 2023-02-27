################################################################################ 
# Name: music_history/scripts/.R
# Created by: Josh Radack
# Date: 2023-02-24
# Purpose: Cleans last.fm user data, incorporates genre information from
#          musicbrainz.
################################################################################

artist_search_res <- readRDS("data/raw/artist_search_20221220.RDS")

# Query artist ID to get genre information
artists <- artist_search_res$artists

queryArtistGenre <- function(artist, artist_id){
  url <- paste0(root, "artist/", artist_id, "?inc=genres+tags&fmt=json")
  res <- GET(url)
  dat <- fromJSON(rawToChar(res$content))$genres
  if(is.data.frame(dat)){
    dat_clean <- data.frame(artist = artist, artist_id = artist_id, genre_name = dat$name)
  } else {
    dat_clean <- data.frame(artist = artist, artist_id = artist_id, genre_name = "")
  }
  return(dat_clean)
}
queryArtistGenre_lim <- limit_rate(queryArtistGenre, rate(n = 1, period = 1))

# Test the function
artist_mbid <- "8d455809-96b3-4bb6-8829-ea4beb580d35" # Phoenix
artist_query <- queryArtistGenre("Phoenix", artist_mbid)

# Get the genres of each artist and combine into a single data frame
artist_genre <- mapply(queryArtistGenre_lim, artists$artist, artists$artist_mbid, SIMPLIFY = FALSE)
artist_genre_df <- do.call("rbind", artist_genre)


# Save resulting data frame
save(artist_genre_df, "data/raw/artist_genre_20230224.Rda")




###############################################################################
library(spotifyr)
library(ratelimitr)

spotify_secrets <-readLines("data/secret.txt")

Sys.setenv(SPOTIFY_CLIENT_ID = spotify_secrets[1])
Sys.setenv(SPOTIFY_CLIENT_SECRET = spotify_secrets[2])
access_token <- get_spotify_access_token()

get_spotify_genre <- function(artist_name) {
  tryCatch(
    expr = {
      # Search spotify for artist and get their spotify URI
      artist_search <- search_spotify(artist_name)
      artist_results <- artist_search$artists$items
      top_artist_uri <- gsub("spotify:artist:", "", artist_results$uri[1])
      
      # Get the artist's information using URI and grab their genre information
      artist_match_info <- get_artist(top_artist_uri)
      artist_match_genre <- artist_match_info$genres
      if(length(artist_match_genre) == 0){
        stop("Couldn't find genre for matching artist")
      }
      return(data.frame(artist = artist_name, genre_name = artist_match_genre))
    },
    error = function(e){
      message(e)
      artist_match_genre <- NA
      return(data.frame(artist = artist_name, genre_name = artist_match_genre))
    }
  )
}
get_spotify_genre_lim <- limit_rate(get_spotify_genre, rate(n = 1, period = 1))

get_spotify_genre_lim("Pet Shimmer")
get_spotify_genre_lim("Pasta Grows on Trees")
get_spotify_genre_lim("サニーデイ・サービス")


artist_search_res <- readRDS("data/raw/artist_search_20221220.RDS")
artists <- artist_search_res$artists


run_get_spotify_genre <- function(artists, n = nrow(artists)) {
  force(n)
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  artist_genre <- sapply(1:n, function(x) {
    print(x)
    res <- get_spotify_genre_lim(artists$artist[x])
    setTxtProgressBar(pb, x)
    return(res)
    }, 
    simplify = FALSE)
  artist_genre_df <- do.call("rbind", artist_genre)
  return(artist_genre_df)
}

artist_genre_df <- run_get_spotify_genre(artists)
save(artist_genre_df, file = "data/raw/artist_genre_20230224.Rda")
