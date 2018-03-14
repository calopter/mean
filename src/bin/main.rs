extern crate reqwest;
extern crate select;
//extern crate futures;
//extern crate hyper;
//extern crate tokio_core;

use select::document::Document;
use select::predicate::Class;
use std::fs::File;
use std::io::{self, Write};
//use futures::{Future, Stream};
//use hyper::Client;
//use tokio_core::reactor::Core;

fn main() {
    let url = "https://songmeanings.com/songs/view/583/";

    for page in 1.. {
        let comments = song_page(url, page);
        if comments.is_empty() {
            break
        } else {
            //comments.extend(new_comments);
            println!("page {}", page);
            write(format!("out/out{}.txt", page), comments);
        }
    }
}

fn song_page(url: &str, page: u8) -> Vec<String> {
    let client = reqwest::Client::new();
    let resp = client
        .post(url)
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

fn write(path: String, comments: Vec<String>) {
   let mut file = File::create(path).unwrap();

   for comment in comments {
       file.write_all(comment.as_bytes()).unwrap();
   }
}
