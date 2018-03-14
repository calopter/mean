extern crate reqwest;
extern crate select;
use select::document::Document;
use select::predicate::Class;
use std::fs::{File, create_dir_all};
use std::io::Write;

struct Song {
    artist: String,
    name: String,
    url: String
}

//creep: "https://songmeanings.com/songs/view/583/"

fn main() {
    let pt = make_song("plastic_trees".to_string(), 573);
    pt.scrape_comments();
}

fn make_song(name: String, url_num: u16) -> Song {
    Song { artist: "radiohead".to_string(), name, 
           url: format!("https://songmeanings.com/songs/view/{}/", url_num)
    }
}

impl Song {
    fn scrape_comments(&self) {
        let path = format!("./out/{}/{}/", &self.artist, &self.name); 
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

    fn song_page(&self, page: u8) -> Vec<String> {
        let client = reqwest::Client::new();
        let resp = client
            .post(&self.url)
            .form(&[("page", page)])
            .send()
            .unwrap();
        assert!(resp.status().is_success());

        Document::from_read(resp)
            .unwrap()
            .find(Class("text"))
            .map(|x| x.text().trim().to_string())
            .filter(|x| *x != "")
            .collect()
    }
}

fn write(path: String, comments: Vec<String>) {
   let mut file = File::create(path).unwrap();

   for comment in comments {
       file.write_all(comment.as_bytes()).unwrap();
   }
}
