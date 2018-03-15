extern crate reqwest;
extern crate select;
use select::document::Document;
use select::predicate::Class;
use std::fs::{File, create_dir_all};
use std::io::Write;
use std::process::Command;

struct Song {
    artist: String,
    name: String,
    url: String
}

fn main() {
    let pt = make_song("plastic_trees".to_string(), 573);
    pt.scrape_comments();
    pt.cleanup();
//    let creep = make_song("creep".to_string(), 583);
//    creep.scrape_comments();
}

fn make_song(name: String, url_num: u16) -> Song {
    Song { artist: "radiohead".to_string(), name, 
           url: format!("https://songmeanings.com/songs/view/{}/", url_num),
    }
}

impl Song {
    fn scrape_comments(&self) {
        let path = format!("out/{}/{}/", &self.artist, &self.name); 
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
    }

    fn cleanup(&self) {
        Command::new("./process.hs")
                .args(&[&self.name])
                .spawn()
                .expect("failed to execute cleanup");
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
