package main

import (
    "os"
    "fmt"
    "bytes"
    "strconv"
    "log"
)

func main(){
    infile, err := os.Open("counter_8.txt")
    if err != nil {
        log.Fatal(err)
    }
    outfile, err := os.Create("counter_8_res.txt",)
    
    if err != nil {
        log.Fatal(err)
    }
    for i := 0; i< 640; i++ {
        text := make([]byte,6)
        infile.Read(text)
        text = bytes.TrimSpace(text)
        
        var str = string(text)
        fmt.Printf("%v] %v\n",i,str)
        addr,err := strconv.ParseUint(str,16,64) 
        if err != nil {
            log.Fatal(err)
        }


        var wr_ptr_p1 uint64 = (((0b111<<3)& addr)>>3)
        var wr_ptr_p2 uint64 = (((0b1111111111<<6)& addr)>>6)
        //fmt.Printf("wr_ptr_p1\n%016b     >> %016b\n%016b:% 3v >> %016b:% 3v\n\n",(0b111<<3),0b111,addr,addr,wr_ptr_p1,wr_ptr_p1)
        //fmt.Printf("wr_ptr_p2\n%016b     >> %016b\n%016b:% 3v >> %016b:% 3v\n\n",(0b1111111111<<6),0b1111111111,addr,addr,wr_ptr_p2,wr_ptr_p2)


        out_str := fmt.Sprintf("%v >> [%v,%v] >> [% 5v,%v] >> %v",str,wr_ptr_p1,wr_ptr_p2,wr_ptr_p1*640,wr_ptr_p2*8,wr_ptr_p1*640+wr_ptr_p2*8)
        //fmt.Println(out_str)
        outfile.WriteString(out_str + "\n")
    }

    infile.Close()
    outfile.Close()
}

