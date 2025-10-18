import cloudscraper

def scrape_page(url):
    scraper = cloudscraper.create_scraper()
    r = scraper.get(url)
    return r.text