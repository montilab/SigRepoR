README
================
Callen & Vanessa
12/10/2020

SigRepo
=======

### Last update

12/10/2020

Contact
-------

Callen Bragdon - `cjoseph@bu.edu`

Vanessa Mengze Li - `vmli@bu.edu`

SigRepo Components
------------------

-   Back-end: MariaDB
-   R functions & R6 object: This package and `OmicSignature` package (see **Installation** below)
-   Front-end: R-Shiny interface
-   API

*See our developing [Shiny app server](http://sigrepo.bu.edu:3838/app) ! (BU VPN required)*

**Installation**
----------------

`devtools::install_github(repo = "montilab/SigRepoR", auth_token = "...")`
`devtools::install_github(repo = "Vanessa104/OmicSignature")`

Please also install `OmicSignature`, which includes the R6 object of `OmicSignature` and `OmicSignatureCollection` to store signatures. Click here for the [vignette](https://vanessa104.github.io/OmicSignature/articles/OmicSig_vignette.html) of `OmicSignature` R6 object.

------------------------------------------------------------------------

Manual of uploading signatures into Database
--------------------------------------------

#### Configuration

Before proceeding with uploading signatures, it's important that you first configure your session to point to which database to upload to, along with where to write files to in your file system.

``` r
configureSigRepo(
    signatureDirectory="/your/directory/path",
    databaseServer="put your database server (IP) address here", 
    databasePort="put your port here. default 3306"
)
```

Now, for downstream queries, you'll be able to establish connections in the future without needing to specify which server to query repeatedly.

Assuming you already have your OmicSignature object created, let's work with uploading.

#### Uploading OmicSignature Object

To upload your object completely to your back-end

``` r
addSignatureWrapper(
    yourObjectOrFileOfObject,
    thisHandle=yourConnectionHandle,
    uploadPath=sys.getenv("signatureDirectory"),
    user="your SigRepo Username"
)
```

executing the above:

-   writes your object file to disk
-   inserts signature metadata into signatures table in the database(addSignature)
-   inserts level2 data from that object into the features\_signatures table in the database(addLevel2).
-   inserts signature-keyword pairs into the keyword\_signatures table in the database(addSignatureKeywords).

#### Uploading OmicSignatureCollection Objects

OmicSignatureCollection Objects are simply a group of Omic Signature objects. You can upload such objects like this:

``` r
addSignatureCollection(
    OmicSignatureCollectionObj, 
    connHandle,
  uploadPath=sys.getenv("signatureDirectory"), 
  thisUser="your SigRepo User Name"
)
```

This function:

-   "unpacks" the OmicSignatureCollection by getting the "OmicSigList" property
-   runs an lapply of the function "addSignatureWrapper" on this list
-   inserts signature-to-collection pairs into the signature-to-collection table in the database(addCollectionSignatures)

If the collection you're uploading doesn't exist as an entry in the collections table of the database, the addCollectionSignatures function will add that collection as a new entry to the collections table before uploading the pairs.
