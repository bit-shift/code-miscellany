#!/usr/bin/env python
import argparse, lxml.html, os, requests, sys

def main():
    parser = argparse.ArgumentParser(
            description="Upload an image to img.tylian.net."
            )
    parser.add_argument('upload_file', type=argparse.FileType('rb'),
            metavar="IMAGEFILE", help="""The image file to upload. Bad things
            happen if this isn't actually an image file, so try not to do that,
            mmkay?""")

    args = parser.parse_args()

    upload_page = requests.get("http://img.tylian.net/")
    if upload_page.status_code != 200:
        sys.stderr.write("SOMETHING FUCKED UP\n")
        sys.stderr.write("{0}: {1}\n".format(
            upload_page.status_code,
            upload_page.content or ""
            )
        )
        sys.exit(1)
    page_data = lxml.html.document_fromstring(upload_page.content)
    postkey = page_data.get_element_by_id("postkey").value

    response = requests.post("http://img.tylian.net", data={
            "postkey": postkey,
            },
        files={
            "fileup": (os.path.basename(args.upload_file.name), args.upload_file),
            }
        )

    response_page = lxml.html.document_fromstring(response.content)

    upload_data = response_page.get_element_by_id("subiste-viendo")

    for link in upload_data.iterlinks():
        for link_attr in filter((lambda x: isinstance(x, str)), link):
            if link_attr.startswith("http://i.tylian.net/"):
                sys.stdout.write(link_attr)
                sys.exit(0)

if __name__ == '__main__':
    main()
