# hewitt_etal_2024/download_original.R
# Output: original/ (the deposited archive, not redistributed in this repo)
#         original_extracted/ (its 186 members, also not redistributed)
# Depends on: original_manifest.csv, original_members_manifest.csv
# Description: Fetch the deposited archive from Harvard Dataverse, verify it, and
#   unpack it. Run this once before running anything in maintained/. Re-running is
#   free: a file already present with the right checksum is not downloaded again.
#
#   This deposit is a single 257 MB zip container rather than a set of individual
#   files, so verification happens twice. original_manifest.csv pins the container,
#   in the format every repository in this program uses; original_members_manifest.csv
#   pins the 186 files inside it, so that a reader can tell an intact unpacking from
#   a partial one without opening the container by hand. That second manifest is not
#   ceremonial here: a partial unpacking of this archive omits the whole of
#   output/processed_data/, and every plotting script then fails on its first read
#   with an error that reads exactly like a deposit shipping no data at all.
#
#   The container manifest carries two checksums. md5_served is the MD5 of the bytes
#   Dataverse returns for ?format=original, which is what this code was written
#   against. md5_published is the checksum Dataverse displays. Here the two agree,
#   but they do not always: other deposits carry published checksums that verify
#   neither the deposited file nor anything derived from it, so verification runs
#   against md5_served plus byte size and any disagreement is reported rather than
#   raised. The file is not tabular, so Dataverse derives no representation of it and
#   ?format=original returns the deposited bytes unchanged.

library(tidyverse)
library(here)

here::i_am("download_original.R")

dataset_doi <- "doi:10.7910/DVN/LBPSSV"
base_url <- "https://dataverse.harvard.edu/api/access/datafile"

manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

dir.create(here::here("original"), showWarnings = FALSE, recursive = TRUE)

# Download what is missing or wrong ----
planned <- manifest |>
  mutate(
    path = here::here("original", file),
    url = str_glue("{base_url}/{dataverse_file_id}?format=original"),
    md5_local = unname(tools::md5sum(path)),
    needs_download = is.na(md5_local) | md5_local != md5_served
  )

walk2(
  planned$url[planned$needs_download],
  planned$path[planned$needs_download],
  function(url, path) download.file(url, destfile = path, mode = "wb", quiet = TRUE)
)

print(str_glue("Downloaded {sum(planned$needs_download)} of {nrow(planned)} files; ",
               "{sum(!planned$needs_download)} already present and verified."))

# Verify the container ----
verified <- planned |>
  mutate(
    md5_downloaded = unname(tools::md5sum(path)),
    bytes_on_disk = file.size(path),
    md5_ok = !is.na(md5_downloaded) & md5_downloaded == md5_served,
    bytes_ok = !is.na(bytes_on_disk) & bytes_on_disk == bytes,
    published_agrees = md5_served == md5_published
  ) |>
  select(file, bytes, bytes_on_disk, bytes_ok, md5_served, md5_downloaded, md5_ok,
         published_agrees)

if (!all(verified$md5_ok & verified$bytes_ok)) {
  print(verified |> filter(!md5_ok | !bytes_ok), n = Inf)
  stop("Checksum or byte size mismatch in original/. Delete the offending file and ",
       "re-run to refetch it from Dataverse.")
}

# original/ must hold the deposit and nothing else ----
# This runs after the checksums, so a deposited file renamed by hand fails on its own
# checksum rather than passing here as an unlisted extra. all.files = TRUE is not
# optional: without it a stray dotfile passes unseen. The strays this deposit invites
# are the Dataverse bulk-download wrapper, which is a second zip holding the first,
# and any tree left behind by unpacking the container in place.
extra <- setdiff(
  list.files(here::here("original"), recursive = TRUE, all.files = TRUE, no.. = TRUE),
  manifest$file
)

if (length(extra) > 0) {
  print(extra)
  stop("original/ holds ", length(extra), " file(s) the manifest does not list. ",
       "Move them elsewhere; original/ is the deposit and only the deposit.")
}

print(str_glue("The container matches md5_served and the deposited byte size, and ",
               "original/ holds nothing else. {sum(!verified$published_agrees)} of ",
               "{nrow(verified)} carry a published checksum that disagrees."))

# Unpack ----
# Everything downstream reads original_extracted/, never original/, so that original/
# holds the deposit and only the deposit. The container is rebuilt from scratch on
# every run and is written to by nothing else: the deposit's own scripts write into
# output/ beside their inputs, and output/processed_data/ is itself deposited, so a
# run inside the extracted tree overwrites deposited members.
extract_dir <- here::here("original_extracted")
unlink(extract_dir, recursive = TRUE)
dir.create(extract_dir, showWarnings = FALSE)
utils::unzip(planned$path[1], exdir = extract_dir)

# Verify the members ----
members <- read_csv(here::here("original_members_manifest.csv"), show_col_types = FALSE) |>
  mutate(
    path = file.path(extract_dir, member),
    md5_extracted = unname(tools::md5sum(path)),
    bytes_extracted = file.size(path),
    ok = !is.na(md5_extracted) & md5_extracted == md5 &
      !is.na(bytes_extracted) & bytes_extracted == bytes
  )

if (!all(members$ok)) {
  print(members |> filter(!ok) |> select(member, bytes, bytes_extracted, md5, md5_extracted),
        n = Inf)
  stop("The unpacked archive does not match the deposited members.")
}

member_extra <- setdiff(
  list.files(extract_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE),
  members$member
)

if (length(member_extra) > 0) {
  print(member_extra)
  stop("original_extracted/ holds ", length(member_extra),
       " file(s) the members manifest does not list.")
}

print(str_glue("All {nrow(members)} members unpacked and verified into original_extracted/."))
print(str_glue("Archive: {dataset_doi}"))
