package main

import (
	"bytes"
	"fmt"
	"image"
	"image/color"
	"image/jpeg"
	"log"
	"os"
	"path"
	"strconv"
	"strings"
)

func main() {
	args := os.Args
	var text_path string
	var out_path string
	if len(args) == 1 {
		text_path = "./test.txt"
		out_path = "./test.jpg"
	} else {
		text_path = args[1]
		text_dir := path.Dir(text_path)
		text_name := strings.ReplaceAll(path.Base(text_path), path.Ext(text_path), "")
		out_path = text_dir + "/" + text_name + ".jpg"
	}
	fmt.Println(text_path, out_path)

	infile, err := os.Open(text_path)
	if err != nil {
		log.Fatal(err)
	}
	defer infile.Close()

	// read first three lines
	var buf bytes.Buffer
	var bit_depth, x_size, y_size uint64
	char := make([]byte, 1)
	for i := 0; i < 3; {
		infile.Read(char)
		if string(char) == "\n" {
			switch i {
			case 0:
				bit_depth, _ = strconv.ParseUint(buf.String(), 10, 64)
			case 1:
				y_size, _ = strconv.ParseUint(buf.String(), 10, 64)
			case 2:
				x_size, _ = strconv.ParseUint(buf.String(), 10, 64)
			}
			i++
			buf.Reset()

		} else {
			buf.WriteByte(char[0])
		}

	}
	fmt.Println(bit_depth, x_size, y_size)
	pixel := make([]byte, 2)
	img := image.NewRGBA(image.Rectangle{image.Point{0, 0}, image.Point{int(x_size), int(y_size)}})

	for i := 0; i < int(y_size); i++ {
		for j := 0; j < int(x_size); j++ {
			var values [3]uint8
			for k := 0; k < int(bit_depth); k++ {
				_, err := infile.Read(pixel)
				if err != nil {
					log.Fatal(err)
				}
				tmp, err := strconv.ParseUint(string(pixel), 16, 8)
				if err != nil {
					log.Fatal(err)
				}
				values[k] = uint8(tmp)

			}
			img.Set(i, j, color.RGBA{values[0], values[1], values[2], 0xff})
		}
		newline := make([]byte, 1)
		infile.Read(newline) //for the /n at the end of a line
	}

	outfile, _ := os.Create(out_path)
	defer outfile.Close()
	jpeg.Encode(outfile, img, nil)
}
