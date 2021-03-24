# hw3-tarasska

This is the FIRST(old) version of the functional programming homework.
The file manager uploads the file system into RAM at the specified path, be careful!

To run the application use `stack run <folder_path>`  
To run the test use `stack test`

The file manager provides the following commands:

`cat <file_path>`                Show file content by path  
`cd  <file|folder_path>`         Change directory  
`create-file <file_name>`        Create file by name in the current directory  
`create-folder <folder_name>`    Create folder by name  
`dir`                            Show current directory content  
`find-file <file_name>`          Find file in (sub)directory by name  
`information <file|folder_path>` Show dir or file information  
`ls <folder_path>`               Show content of the directory  
`quit`                           Write data to real file system and quit  
`remove <folder|file_path>`      Remove directory(recursively) or file by path  
`tree <folder_path>`             Shows the filesystem tree by path  
`write-file <file_path> <text>`  Write text to file by path

The entered data is split into parts by space, but you can include the space 
inside the path or name by enclosing the text in double quotes, then the spaces 
between the two quotes will be considered part of the text.  