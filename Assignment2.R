

install.packages("paws")
install.packages("purrr")
install.packages("tibble")
install.packages("readr")
install.packages("magick")


Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIA5JJX7KGSS27HKPSC",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_REGION" = "ap-southeast-2"
)
#set up system environment.

library(paws) 
library(purrr)
library(tibble)
library(readr)
library(magick)
#above call required libraries

# call 's3' from cloud.
s3 <- s3()

buckets <- s3$list_buckets() #assign list of buckets of s3 to "buckets" name.
length(buckets$Buckets) #show how many buckets you have.

#create a bucket in  cloud "Amazon".
s3$create_bucket(Bucket = "r-and-rekognition-bucket2022",
                 CreateBucketConfiguration = list(
                   LocationConstraint = "ap-southeast-2"))

# create a tibble of name and creation data of the buckets.
buckets <- map_df(buckets[[1]],
                  ~tibble(name = .$Name, creationDate = .$CreationDate))
buckets #see your buckets (name & creation date)

#use bucket "r-and-rekognition-bucket2022" for the assignment. 
#For the purpose of the assignment,from here onward the code is required.

my_bucket <- buckets$name[buckets$name == "r-and-rekognition-bucket2022"]

#put object to your bucket
s3$put_object(Bucket = my_bucket, Body = read_file_raw("teslatxt.jpg") , Key = "teslatxt.jpg")

#red contents/text of the object in the bucket.
bucket_objects <- s3$list_objects(my_bucket) %>% .[["Contents"]] %>% map_chr("Key")

#call Rekognition and assign to name "rekognition".
rekognition <- rekognition()

#Referencing an image in an Amazon s3 bucket
resp <- rekognition$detect_text(
  Image = list(
    S3Object = list(
      Bucket = my_bucket,
      Name = bucket_objects
    )
  )
)

#Parsing the response
resp %>%
  .[["TextDetections"]] %>%
  keep(~.[["Type"]] == "WORD") %>%
  map_chr("DetectedText")



# Load raw images into memory 
masoud <- readr::read_file_raw("moa1.jpg")
suspects <- readr::read_file_raw("moa.jpg")

#send image to the compare faces endpoint

resp <- rekognition$compare_faces(
  SourceImage = list(
    Bytes = masoud
  ),
  TargetImage = list(
    Bytes = suspects
  )
)

length(resp$UnmatchedFaces)

length(resp$FaceMatches)

resp$FaceMatches[[1]]$Similarity

#convert raw image into a magmick object
suspects <- image_read(suspects)

#Extract face match from the response
match <- resp$FaceMatches[[1]]

#Calculate bounding box properties
width <- match$Face$BoundingBox$Width * image_info(suspects)$width
height <- match$Face$BoundingBox$Height * image_info(suspects)$height
left <- match$Face$BoundingBox$Left * image_info(suspects)$width
top <- match$Face$BoundingBox$Top * image_info(suspects)$height
#add bounding box to suspects image
image <- suspects %>%
  image_draw()

rect(left, top, left + width, top + height, border = "red", lty = "dashed", lwd = 5)

image

////////////////////////////////////////
  
  
# Loop through the files in the specified folder, add and index them in the collection
for(f in filenames) {
  imgFile = paste(path,f,sep="/")
  # Get the person name, which is embedded in the last file path folder name
  imgName = unlist(strsplit(f,split="/"))[[1]]
  # Add the photos and the name to the AWS collection
  svc$index_faces(CollectionId="photos-r", Image=list(Bytes=imgFile), ExternalImageId=imgName, DetectionAttributes=list())
}

svc$list_faces(CollectionId = "photos-r")

### Label and identify the face of a new photo ###

# Grab a new photo with multiple faces
group_photo = "~/Desktop/face_detection/img1.JPG"
group_file_name = unlist(strsplit(group_photo,split="/"))[[4]]  # used for writing out annotated file

# Read the photo using magick
img = image_read(group_photo)

# Get basic info about the photo to be used for annotation
inf = image_info(img)

# Detect the faces in the image and pull all attributes associated with faces
o = svc$detect_faces(Image=list(Bytes=group_photo), Attributes="ALL")

# Get the face details
all_faces = o$FaceDetails
length(all_faces)


### For each face in photo, draw a rectange with the name and emotions ###
new.img = img  # Duplicate the original image
people_df <- NULL

for(face in all_faces) {
  
  # Get emotions from AWS rekognition model
  emo_label = ""
  for(emo in face$Emotions) {
    emo_label = paste(emo_label, emo$Type, " = ", round(emo$Confidence, 2), "\n", sep="")
  }
  
  # Get ages from AWS rekognition
  age_label = ""
  for(age in list(face$AgeRange)) {
    age_label = paste(age_label, "AGE ESTIMATE: = ", (age$Low+age$High)/2, "\n", sep="")
  }
  
  # Grab genders from AWS rekognition
  #gender_label = ""
  #for(gndr in list(face$Gender)) {
  #  gender_label = paste(gender_label, gndr$Value, " = ", round(gndr$Confidence, 2), "\n", sep="")
  #}
  
  # Append all lists together
  final_label = ""
  final_label <- rbind(emo_label, age_label)
  final_label <- paste(final_label, collapse = '')
  
  # Identify the coordinates of the face. Note that AWS returns percentage values of the total image size. This is
  # why the image info object above is needed
  box = face$BoundingBox
  image_width=inf$width
  image_height=inf$height
  x1 = box$Left*image_width
  y1 = box$Top*image_height
  x2 = x1 + box$Width*image_width
  y2 = y1 + box$Height*image_height  
  
  # Create a subset image in memory that is just cropped around the focal face
  img.crop = image_crop(img, paste(box$Width*image_width,"x",box$Height*image_height,"+",x1,"+",y1, sep=""))
  img.crop = image_write(img.crop, path = NULL, format = "png")
  
  # Search in a specified collection to see if we can label the identity of the face is in this crop
  o = svc$search_faces_by_image(CollectionId="photos-r",Image=list(Bytes=img.crop), FaceMatchThreshold=70)
  
  # Create a graphics device version of the larger photo that we can annotate
  new.img = image_draw(new.img)
  
  # If the face matches something in the collection, then add the name to the image
  if(length(o$FaceMatches) > 0) {
    faceName = o$FaceMatches[[1]]$Face$ExternalImageId
    faceConfidence = round(o$FaceMatches[[1]]$Face$Confidence,3)
    print(paste("Detected: ", faceName, sep=""))
    
    # Annotate with the name of the person
    text(x=x1+(box$Width*image_width)/2, y=y1-20, faceName, adj=0.5, cex=3, col="green")
  }
  
  # Draw a rectangle around the face
  rect(x1,y1,x2,y2, border="red", lty="dashed", lwd=5)   
  
  # Annotate the photo with the emotions information
  text(x=x1+(box$Width*image_width)/2, y=y1+50, final_label, pos=1, cex=1.5, col="red")     
  
  # Create a dataframe of individual data appended together
  individual_emotion_df <- do.call(rbind.data.frame, face$Emotions)
  
  individual_emotion_df <- individual_emotion_df %>% 
    spread(Type, Confidence) %>%
    add_column(faceName)
  individual_emotion_df$image <- strsplit(group_file_name, ".JPG")
  
  individual_emotion_df <- individual_emotion_df%>%
    select(faceName, image, everything()) # move faceName to beginning
  
  individual_age_df <- data.frame(face$AgeRange)
  colnames(individual_age_df) <- c("age_low", "age_high")
  
  individual_df <- cbind(individual_emotion_df, individual_age_df)
  
  people_df <- rbind(individual_df, people_df)
  
}
dev.off()

people_df$age_est <- (people_df$age_low + people_df$age_high)/2
names(people_df) <- tolower(names(people_df))
head(people_df)

# Write the image out to file 
image_write(new.img, path=paste0("~/Desktop/face_detection/annotated/annotated_", group_file_name))