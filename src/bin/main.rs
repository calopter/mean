#![feature(iterator_step_by)]

extern crate reqwest;
extern crate select;
extern crate rayon;
use select::document::Document;
use select::predicate::{Class, Name, Predicate};
use std::fs::{File, create_dir_all};
use std::path::Path;
use std::io::Write;
use std::process::Command;
use rayon::prelude::*;

#[derive(Debug)]
struct Song {
    artist_name: String,
    name: String,
    url: String
}

#[derive(Debug)]
struct Artist {
    name: String,
    url: String,
    songs: Vec<Song>
}

fn main() {
    let mut radiohead = make_artist("radiohead".to_string(), 200);
    radiohead.scrape_urls();
    radiohead.scrape_songs();
}

fn make_artist(name: String, url_num: u16) -> Artist {
    Artist { name, url: format!("https://songmeanings.com/artist/view/songs/{}/", url_num), songs: Vec::new() }
}

impl Artist {
    fn scrape_songs(&self) {
        &self.songs.par_iter()
                   .for_each(|s| s.scrape_comments());
        cleanup(&format!("out/{}", &self.name), "1", &self.name);
    }

    fn scrape_urls(&mut self) {
        let resp = reqwest::get(&self.url).unwrap();
        assert!(resp.status().is_success());
        let document = Document::from_read(resp).unwrap();
        let links = document.find(Name("td")
                            .descendant(Name("a")))
                            .step_by(2);

        for link in links {
            let song = Song { artist_name: self.name.to_string(),
                           name: link.text().replace("/", "_"),
                           url: format!("https:{}", link.attr("href").unwrap())
            };
//            println!("{:?}", &song);
            self.songs.push(song);
        }
    }
}

impl Song {
    fn scrape_comments(&self) {
        let path = format!("out/{}/{}", &self.artist_name, &self.name); 
        if Path::new(&format!("{}.txt", &path)).exists() { return; };

        create_dir_all(&path).unwrap();

        for page in 1.. {
            let comments = self.song_page(page);
            if comments.is_empty() {
                break
            } else {
                println!("{}: page {}", &self.name, page);
                write(format!("{}/out{}.txt", path, page), comments);
            }
        }
        cleanup(&path, "0", &self.name);
    }

    fn song_page(&self, page: u8) -> String {
        let client = reqwest::Client::new();
        let resp = client
            .post(&self.url)
            .form(&[("page", page)])
            .send()
            .unwrap();
        assert!(resp.status().is_success());
        
        let document = Document::from_read(resp).unwrap();
        let mut comments = String::new();

        for node in document.find(Class("text")) {
            for n in node.children() {
                match n.as_text() {
                    Some(c) => comments.push_str(&pad(c)),
                    None => continue
                }
            }
        }
        comments
    }
}

fn cleanup(path: &str, flag: &str, name: &str) { 
    //combine .txt files in dir, non "1" flag deletes
    Command::new("./process.hs") 
            .args(&[path, flag])
            .spawn()
            .expect("failed to execute cleanup");
    println!("{} scrape completed", name);
} 

fn pad(s: &str) -> String {
    match s.trim() {
        "" => "".to_string(),
        comment => format!("{} ", comment)
    }
}

fn write(path: String, comments: String) {
   let mut file = File::create(path).unwrap();
   file.write_all(comments.as_bytes()).unwrap();
}
