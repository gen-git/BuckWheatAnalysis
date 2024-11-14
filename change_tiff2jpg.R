library(imager)

# Target folder
tgt_folder = "Plot"

listfile = list.files(tgt_folder)
x1 = grep("tiff", listfile)

k = 1
for(i in x1){
  tgt_path = paste0(tgt_folder,"/",listfile[i])
  
  img = load.image(tgt_path)
  
  file_name = sub(".tiff",".jpg",listfile[i])
  out_path = paste0(tgt_folder,"/",file_name)
  imager::save.image(img, out_path)
  
  cat(k,"/",length(x1),"was finished","\n")
  k = k + 1
}