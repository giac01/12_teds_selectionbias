#!/usr/bin/env python3
"""
Builds a single combined HTML supplement by extracting specific sections
from already-rendered Quarto HTML files. No R code is re-run.

Usage: python3 build_supplement.py
Output: 0_OnlineSupplement_combined.html (in the same directory)
"""

from bs4 import BeautifulSoup
from pathlib import Path
import re

BASE_DIR = Path(__file__).parent

# ---------------------------------------------------------------------------
# CONFIGURATION
# Each entry is either:
#   ("heading", "Heading text", level)          -- inserts an <h1>/<h2>/<h3>
#   ("section", "file.html", "section-id")      -- embeds a section by ID
#   ("note", "Some HTML note text")             -- inserts a styled note box
# ---------------------------------------------------------------------------
STRUCTURE = [
    ("heading", "Changes from the Pre-registration", 1),
    ("section", "99_preregistration_changes.html", "quarto-document-content"),

    ("heading", "Descriptive Statistics", 1),
    ("section", "11_descriptives.html", "all-measures-used-in-study"),
    ("section", "11_descriptives.html", "age-table"),
    ("section", "11_descriptives.html", "education"),

    ("heading", "Research Question 1: Predictors of Participation", 1),
    ("section", "1_rq1_predictors_of_participation.html", "full-list-of-imputation-variables-rq1"),
    ("section", "1_rq1_predictors_of_participation.html", "twin-level-logistic-regression-coefficients"),
    ("section", "1_rq1_predictors_of_participation.html", "plot-marginal-effects-of-maternal-education"),
    ("section", "1_rq1_predictors_of_participation.html", "plot-marginal-effects-of-ethnicity"),

    ("heading", "Research Question 2: Effects of Attritioning", 1),
    ("heading", "Toddlerhood Outcomes", 2),
    ("section", "2_rq2_results.html", "create-gt-results-table"),
    ("section", "2_rq2_results.html", "correlations-results-table"),
    ("section", "2_rq2_results.html", "ace-estimates-differences-table-complete"),
    ("section", "2_rq2_results.html", "distribution-of-maternal-education-full-sample-vs.-y26-webtest-participants"),

    ("heading", "Adolescent Outcomes", 2),
    ("section", "6_rq6_2_results.html", "means-smds-variances"),
    ("section", "6_rq6_2_results.html", "gt-results-table-1"),
    ("section", "6_rq6_2_results.html", "table---changes-in-ace-estimates"),

    ("heading", "Research Question 3: Evaluating Correction Methods", 1),
    ("section", "8_rq6_3_imputation_results.html", "table-of-all-variables-used-in-multiple-imputation-rq3"),
    ("heading", "Means & Variances", 2),
    ("section", "6_rq6_2_results.html", "means-smds-variances"),
    ("section", "7_rq6_2_weighting_results.html", "gt-results-table"),
    ("section", "8_rq6_3_imputation_results.html", "gt-results-table"),
    ("heading", "Correlations", 2),
    ("section", "6_rq6_2_results.html", "gt-results-table-1"),
    ("section", "7_rq6_2_weighting_results.html", "gt-results-table-1"),
    ("section", "8_rq6_3_imputation_results.html", "gt-results-table-1"),
    ("heading", "ACE Estimates", 2),
    ("section", "6_rq6_2_results.html", "table---changes-in-ace-estimates"),
    ("section", "7_rq6_2_weighting_results.html", "table---changes-in-ace-estimates"),
    ("section", "8_rq6_3_imputation_results.html", "table---changes-in-ace-estimates"),
    ("section", "8_rq6_3_imputation_results.html", "calculation-for-sample-means"),
    ("section", "8_rq6_3_imputation_results.html", "table-of-all-variables-used-in-multiple-imputation-rq3"),

    ("heading", "Discussion", 1),
    ("section", "11_descriptives.html", "relationship-between-maternal-education-and-child-outcomes"),
]

# ---------------------------------------------------------------------------


def load_soup(filename: str, cache: dict) -> BeautifulSoup:
    if filename not in cache:
        path = BASE_DIR / filename
        if not path.exists():
            raise FileNotFoundError(f"HTML file not found: {path}")
        with open(path, "r", encoding="utf-8") as f:
            cache[filename] = BeautifulSoup(f.read(), "html.parser")
    return cache[filename]


def extract_section(soup: BeautifulSoup, section_id: str) -> str:
    el = soup.find(id=section_id)
    if el is None:
        return f'<div class="missing-section"><em>Section not found: #{section_id}</em></div>'
    # Quarto wraps content in <section>; return the whole element
    return str(el)


def get_head_assets(soup: BeautifulSoup) -> str:
    """Extract all <style>, <link>, and <script> tags from <head>."""
    head = soup.find("head")
    if not head:
        return ""
    parts = []
    for tag in head.children:
        if hasattr(tag, "name") and tag.name in ("style", "link", "script"):
            parts.append(str(tag))
    return "\n".join(parts)


def build_toc(structure: list) -> str:
    items = []
    current_level = 1
    for entry in structure:
        if entry[0] == "heading":
            _, text, level = entry
            anchor = re.sub(r"[^a-z0-9]+", "-", text.lower()).strip("-")
            indent = "&nbsp;" * (level - 1) * 4
            items.append(f'<li>{indent}<a href="#{anchor}">{text}</a></li>')
            current_level = level
        elif entry[0] == "section":
            _, filename, section_id = entry
            label = section_id.replace("-", " ").title()
            indent = "&nbsp;" * current_level * 4
            items.append(f'<li>{indent}<a href="#{section_id}">{label}</a></li>')
    return "<ul>\n" + "\n".join(items) + "\n</ul>"


def main():
    cache = {}
    body_parts = []

    # Use the first HTML file's head for CSS/JS
    first_html = next(e[1] for e in STRUCTURE if e[0] == "section")
    head_soup = load_soup(first_html, cache)
    head_assets = get_head_assets(head_soup)

    toc_html = build_toc(STRUCTURE)

    for entry in STRUCTURE:
        kind = entry[0]

        if kind == "heading":
            _, text, level = entry
            anchor = re.sub(r"[^a-z0-9]+", "-", text.lower()).strip("-")
            body_parts.append(f'<h{level} id="{anchor}">{text}</h{level}>')

        elif kind == "section":
            _, filename, section_id = entry
            try:
                soup = load_soup(filename, cache)
                html = extract_section(soup, section_id)
                body_parts.append(f'<div class="extracted-section" data-source="{filename}#{section_id}">{html}</div>')
                print(f"  OK  {filename}#{section_id}")
            except FileNotFoundError as e:
                body_parts.append(f'<div class="missing-section"><em>{e}</em></div>')
                print(f"  MISSING  {e}")

        elif kind == "note":
            _, html_text = entry
            body_parts.append(f'<div class="note-box">{html_text}</div>')

    combined_html = f"""<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Supplementary Materials — Selection Bias in TEDS</title>
  {head_assets}
  <style>
    body {{ max-width: 960px; margin: 0 auto; padding: 20px 40px; font-family: sans-serif; }}
    .extracted-section {{ border-left: 3px solid #dee2e6; padding-left: 16px; margin: 24px 0; }}
    .note-box {{ background: #fff3cd; border-left: 4px solid #ffc107; padding: 10px 15px; margin: 15px 0; border-radius: 0 5px 5px 0; }}
    .missing-section {{ color: red; font-style: italic; }}
    #toc {{ background: #f8f9fa; padding: 16px 24px; border-radius: 6px; margin-bottom: 32px; }}
    #toc ul {{ list-style: none; padding-left: 0; margin: 0; line-height: 2; }}
    h1 {{ border-bottom: 2px solid #3498db; padding-bottom: 8px; }}
    h2, h3 {{ color: #2c3e50; }}
  </style>
</head>
<body>
  <h1 style="font-size:1.8em;">Supplementary Materials for Selection Bias in TEDS Paper</h1>
  <div id="toc">
    <strong>Contents</strong>
    {toc_html}
  </div>
  <hr>
  {"<hr>".join(body_parts)}
</body>
</html>"""

    out_path = BASE_DIR / "0_OnlineSupplement_combined.html"
    with open(out_path, "w", encoding="utf-8") as f:
        f.write(combined_html)
    print(f"\nWritten to: {out_path}")


if __name__ == "__main__":
    main()
