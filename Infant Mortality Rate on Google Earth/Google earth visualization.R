### Part 1.  Create the data frame from XML file

### Functions you'll want to use: xmlParse(), xmlRoot(), xpathSApply(), xmlGetAttr().
### It also might make it easier to use: xmlToList(), merge().

### (a) Load the data frame called LatLon from data.rda. 
load("~/Desktop/data.rda")

### (b) Download the gzipped XML factbook document from
### http://jmatchparser.sourceforge.net/factbook/
### and create an XML "tree" in R 
install.packages("XML")
library("XML")
library("methods")
Parsefile <- xmlParse("/Users/glen/Desktop/factbook.xml")

### (c) Use XPath to extract the infant mortality and the CIA country codes from the XML tree
node <- getNodeSet(Parsefile,"//field[@name='Infant mortality rate']/rank")
infant_mortality <- as.numeric(sapply(node,xmlGetAttr,"number"))
CIA.Codes <- sapply(node,xmlGetAttr,"country")

### (d) Create a data frame called IM using this XML file.
### The data frame should have 2 columns: for Infant Mortality and CIA.Codes.
IM <- data.frame(infant_mortality,CIA.Codes)
IM

### (e) Extract the country populations from the same XML document
### Create a data frame called Pop using these data.
### This data frame should also have 2 columns, for Population and CIA.Codes.
node2 <- getNodeSet(Parsefile,"//field[@name='Population']/rank")
Population <- as.numeric(sapply(node2,xmlGetAttr,"number"))
CIA.Codes2 <- sapply(node2,xmlGetAttr,"country")
Pop <- data.frame(Population,CIA.Codes2)
Pop
### (f) Merge the two data frames to create a data frame called IMPop with 3 columns:
### IM, Pop, and CIA.Codes
IMPop <- merge(IM,Pop,by.x ='CIA.Codes',by.y = 'CIA.Codes2')
head(IMPop)
head(LatLon)
### (g) Now merge IMPop with LatLon (from newLatLon.rda) to create a data frame called AllData that has 6 columns
### for Latitude, Longitude, CIA.Codes, Country Name, Population, and Infant Mortality
### (please check lat,long are not reversed in the file)
IMPop$CIA.Codes <- toupper(IMPop$CIA.Codes)
AllData <- merge(IMPop,LatLon,by.x = 'CIA.Codes',by.y = 'CIA.Codes')
head(AllData)
### Part 2.  Create a KML document for google earth visualization.
### Make the KML document with stucture described in hw7_Intro.pdf.  You can use the addPlacemark function below to make
### the Placemark nodes, for which you need to complete the line for the Point node and
### figure out how to use the function.

makeBaseDocument = function(doc){
  ### This code creates the template for KML document 
  doc = newXMLDoc(namespaces = c("http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd"))
  root = newXMLNode("kml",doc = doc)
  document = newXMLNode("Document",parent = root)
  newXMLNode("Name","Country fact",parent = document)
  newXMLNode("Description","Infant Mortality",parent = document)
  Folder = newXMLNode("Folder",parent = document)
  newXMLNode("Name","CIA Factbook",parent = Folder)
  # LookAt = newXMLNode("LookAt",parent = document)
  # newXMLNode("longitude",parent = LookAt)
  # newXMLNode("latitude",parent = LookAt)
  # newXMLNode("altitude",parent = LookAt)
  # newXMLNode("title",parent = LookAt)
  # newXMLNode("heading",parent = LookAt)
  # newXMLNode("altitudemode",parent = LookAt)
  return(doc)
}

addPlacemark = function(lat, lon, ctryCode, ctryName, pop, infM, parent, 
                        inf1, pop1, style = FALSE)
{
  pm = newXMLNode("Placemark", 
                  newXMLNode("name", ctryName), attrs = c(id = ctryCode), 
                  parent = parent)
  newXMLNode("description", paste(ctryName, "\n Population: ", pop, 
                                  "\n Infant Mortality: ", infM, sep =""),
             parent = pm)

  newXMLNode("Point",newXMLNode("coordinates",paste(lon,lat,0,sep = ",")),parent = pm)

### You need to fill in the code for making the Point node above, including coordinates.
### The line below won't work until you've run the code for the next section to set up
### the styles.

  if(style) newXMLNode("styleUrl", paste("#YOR", inf1, "-", pop1, sep = ''), parent = pm)
}


### Use the two functions that I just implemented to created the KML document and save it 
### as 'Part2.kml'. open it in Google Earth. ( will need to install Google Earth.)  
### It should have pushpins for all the countries.  

Parsefile2 <- makeBaseDocument(doc)
Folder <- getNodeSet(Parsefile2,"//Folder")
for (i in 1:length(AllData[,1])){
  addPlacemark(AllData$Latitude[i], AllData$Longitude[i], AllData$CIA.Codes[i], 
               AllData$Country.Name[i],AllData$Population[i], 
               AllData$infant_mortality[i], Folder)
}

saveXML(Parsefile2,file = "Part2.kml")

### Part 3.  Add Style to your KML
### Now going to make the visualizatiion a bit fancier. To be more specific, instead of pushpins, we
### want different circle labels for countris with size representing population and the color representing  
### the infant motality rate.
### Pretty much all the code is given to you below to create style elements.
### Here, I just need to figure out what it all does.

### Start fresh with a new KML document, by calling makeBaseDocument()

doc2 <- makeBaseDocument(doc)

### The following code is an example of how to create cut points for 
### different categories of infant mortality and population size.
### Figure out what cut points you want to use and modify the code to create these 
### categories.
head(AllData)
infCut = cut(AllData$infant_mortality, breaks = c(Min,firstquartile,median,thirdquartile,Max))
infCut = as.numeric(infCut)

Min <- as.numeric(quantile(AllData$infant_mortality,0))
firstquartile <- as.numeric(quantile(AllData$infant_mortality,0.25))
median <- as.numeric(quantile(AllData$infant_mortality,0.5))
thirdquartile <- as.numeric(quantile(AllData$infant_mortality,0.75))
Max <- as.numeric(quantile(AllData$infant_mortality,1))


popCut = cut(AllData$Population, breaks = seq(5000,200241024,length.out = 5))
popCut = as.numeric(popCut)
table(popCut)

### Now figure out how to add styles and placemarks to doc2
### use the addPlacemark function with style = TRUE
### Below is code to make style nodes. 

### Figure out what scales to use for the sizes of your circles. Try different 
### setting of scale here.

scale = c(2,4,6,8,10) #Try your scale here for better visualization
colors = c("blue","green","yellow","orange","red")

addStyle = function(col1, pop1, parent, DirBase, scales = scale)
{
  st = newXMLNode("Style", attrs = c("id" = paste("YOR", col1, "-", pop1, sep="")), parent = parent)
  newXMLNode("IconStyle", 
             newXMLNode("scale", scales[pop1]), 
             newXMLNode("Icon", paste(DirBase, "color_label_circle_", colors[col1], ".png", sep ="")), parent = st)
}


root2 = xmlRoot(doc2)
DocNode = root2[["Document"]]


for (k in 1:5)
{
  for (j in 1:5)
  {
    addStyle(j, k, DocNode, 'color_label_circle/')
  }
}

### Need to figure out what order to call addStyle() and addPlacemark()
### so that the tree is built properly. May need to adjust the code to call the png files

Folder2 <- getNodeSet(doc2,"//Folder")
for (i in 1:length(AllData[,1])){
  addPlacemark(AllData$Latitude[i], AllData$Longitude[i], AllData$CIA.Codes[i], 
               AllData$Country.Name[i],AllData$Population[i], 
               AllData$infant_mortality[i], Folder2,infCut[i],popCut[i],style = TRUE)
}

saveXML(doc2,file = "Part3.kml")

### Finally, save KML document, call it Part3.kml and open it in Google Earth to 
### verify that it works.  For this assignment, you only need to submit your code, 
### nothing else.  You can assume that the grader has already loaded data.rda.

