#!/usr/bin/env python
import logging, lxml.html, os, random, requests, sys, time

def get_or_die(url):
    result = requests.get(url)
    if result.status_code == 200:
        return result.content
    else:
        logging.error("{{{:d}}}\t{!r}\n{}\n".format(
            result.status_code,
            url,
            result.content or "<< No response body >>"
            )
        )
        sys.exit(1)

def get_etree_or_die(url):
    return lxml.html.document_fromstring(get_or_die(url))


def main():
    logging.basicConfig(format="[%(levelname)s] %(message)s", level=logging.INFO)

    starting_page = get_etree_or_die("http://skin-horse.com/2007/12302007/")
    archive_list = starting_page.find_class("archive-dropdown")[0]

    archive_items = []
    for option in archive_list.getchildren():
        title = option.text.replace("\xa0", "")
        url = option.attrib["value"]
        if url not in ("", "http://skin-horse.com/category/skin-horse/"):
            strip_count = int(title.split("(")[-1][:-1])
            title = "(".join(title.split("(")[:-1])[:-1]
            archive_items.append((title, url, strip_count))

    try:
        os.mkdir("skin-horse")
        logging.info("Created base directory.")
    except OSError as e:
        if e.errno != 17: # not "file exists"
            raise(e)
        else:
            logging.info("Base directory exists, skipping creation.")

    for idx, (title, url, strip_count) in enumerate(archive_items):
        try:
            os.mkdir("skin-horse/{:02d} - {}".format(idx, title))
            logging.info("Created directory for storyline {}.".format(idx))
        except OSError as e:
            if e.errno != 17: # not "file exists"
                raise(e)
            else:
                logging.info("Directory for storyline {} exists, checking strip count.".format(idx))
                if len(os.listdir("skin-horse/{:02d} - {}".format(idx, title))) == strip_count:
                    logging.info("Strip count unchanged, skipping storyline.")
                    continue

        category_page = get_etree_or_die(url)
        strip_list = category_page.find_class("comicthumbwrap")
        logging.info("Got strip list.")
        for strip_idx, strip in enumerate(strip_list, 1):
            if os.path.exists("skin-horse/{:02d} - {}/{:03d}.jpg".format(idx, title, strip_idx)):
                logging.info("Strip {:d} exists, skipping fetch.".format(strip_idx))
            else:
                strip_page = get_etree_or_die(next(strip.iterlinks())[2])
                image_url = list(strip_page.find_class("comicpane")[0].iterlinks())[1][2]
                with open("skin-horse/{:02d} - {}/{:03d}.jpg".format(idx, title, strip_idx), "wb") as strip_file:
                    strip_file.write(get_or_die(image_url))
                logging.info("Saved strip {:d}.".format(strip_idx))
                time.sleep(random.randint(2, 6))  # so the server isn't hammered


if __name__ == '__main__':
    main()
