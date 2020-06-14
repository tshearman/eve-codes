#!/usr/bin/env python3

import requests
import re
import os
import argparse

from retrying import retry
from bs4 import BeautifulSoup


def parse(url, parser="html.parser"):
    dump = requests.get(url)
    return BeautifulSoup(dump.content, parser)


def parse_href(tag):
    return tag.attrs["href"]


def sde_match(s):
    return (s[:4] == "sde-" and "TRANQUILITY/" in s) or ()


def valid_file_url(url):
    return len(url.split("/")[-1]) > 0


def extract_file(url):
    return url.split("/")[-1]


@retry(stop_max_attempt_number=10,
       wait_exponential_multiplier=1000,
       wait_exponential_max=10000)
def download(url):
    print(f"Downloading\n\turl: {url}")
    r = requests.get(url, allow_redirects=True)
    print("\tComplete.")
    return r


def write(r, to):
    with open(to, "wb+") as f:
        print(f"Writing\n\tto: {to}")
        f.write(r.content)
        print("\tComplete.")


def download_if_not_exists(url, dest):
    dest = dest if dest[-1] == "/" else f"{dest}/"
    if not os.path.exists(dest):
        print(f"Creating directory: {dest}")
        os.mkdir(dest)

    file = extract_file(url)
    loc = dest + file
    if not os.path.exists(loc):
        r = download(url)
        write(r, loc)


def collect_all(base_url, store_folder):
    dump = parse(base_url)
    folders = sorted(filter(sde_match, map(parse_href, dump.find_all("a"))), reverse = True)
    for folder in folders:
        folder_content = parse(base_url + folder)
        files = map(parse_href, folder_content.find_all("a"))
        files = filter(lambda x: "." in x, files)
        for f in files:
            url = base_url + folder + f
            dest = store_folder + folder
            download_if_not_exists(url, dest)


parser = argparse.ArgumentParser(description="""
Collect eve online static dumps
from url (default: https://www.fuzzwork.co.uk/dump/)
""")

parser.add_argument('--url', dest='url', type=str,
                  default="https://www.fuzzwork.co.uk/dump/",
                  help='url to download the static dumps from')

parser.add_argument('--dest', type=str, dest='dest',
                  default="/data/",
                  help="folder destination to store the static dumps")

args = parser.parse_args()

collect_all(args.url, args.dest)
