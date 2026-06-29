#!/usr/bin/env python3
"""
Builds a single combined HTML supplement by extracting specific sections
from already-rendered Quarto HTML files. No R code is re-run.

Usage: python3 build_supplement.py
Output: 0_supplement.html (in the same directory)
"""

from bs4 import BeautifulSoup
from pathlib import Path
import re

BASE_DIR = Path(__file__).parent

# ---------------------------------------------------------------------------
# CONFIGURATION
# Each entry is either:
#   ("heading", "Heading text", level)                           -- inserts an <h1>/<h2>/<h3>
#   ("section", "file.html", "section-id", "TOC label")         -- embeds a section by ID
#   ("image",   "path/to/image.png", "Caption", "TOC label")    -- embeds an image with caption
#   ("note", "Some HTML note text")                              -- inserts a styled note box
# ---------------------------------------------------------------------------
STRUCTURE = [
    ("section", "99_preregistration_changes.html", "quarto-document-content", "Pre-registration Changes"),

    ("heading", "Descriptive Statistics", 1),
    ("section", "11_descriptives.html", "descriptive-statistics-all-study-variables", "Study Variables"),
    ("section", "11_descriptives.html", "participant-ages-at-each-timepoint", "Participant Ages"),
    ("section", "11_descriptives.html", "maternal-education-distribution", "Maternal Education"),

    ("heading", "Research Question 1: Predictors of Participation", 1),
    ("section", "1_rq1_predictors_of_participation.html", "full-list-of-imputation-variables-rq1", "Variables in Imputation Model"),
    ("section", "1_rq1_predictors_of_participation.html", "twin-level-logistic-regression-coefficients", "Logistic Regression Coefficients"),
    ("section", "1_rq1_predictors_of_participation.html", "rq1-marginal-effects-of-maternal-education-on-participation", "Education Marginal Effects Plot"),
    ("section", "1_rq1_predictors_of_participation.html", "rq1-marginal-effects-of-ethnicity-on-participation", "Ethnicity Marginal Effects Plot"),

    ("heading", "Research Question 2: Effects of Attritioning", 1),
    ("heading", "Toddlerhood Outcomes - Attritioned vs Original Estimates (Bias)", 2),
    ("section", "2_rq2_results.html", "toddlerhood-outcomes-means-smds-and-variances", "Means and Variances"),
    ("section", "2_rq2_results.html", "toddlerhood-outcomes-correlations", "Correlations"),
    ("section", "2_rq2_results.html", "toddlerhood-outcomes-ace-estimates-and-differences", "ACE Estimates"),

    ("heading", "Adolescent Outcomes - Attritioned vs Original Estimates (Bias)", 2),
    ("section", "6_rq6_2_results.html", "adolescent-outcomes-means-smds-and-variances", "Means and Variances"),
    ("section", "6_rq6_2_results.html", "adolescent-outcomes-correlations", "Correlations"),
    ("section", "6_rq6_2_results.html", "adolescent-outcomes-attritioned-vs.-original-ace-estimates", "ACE Estimates"),

    ("heading", "Research Question 3: Evaluating Correction Methods", 1),
    ("section", "8_rq6_3_imputation_results.html", "table-of-all-variables-used-in-multiple-imputation-rq3", "Imputation Variables & FMI"),
    ("heading", "Means, Variances & Correlations", 2),
    ("section", "9_rq6_combined_bias_reduction_est.html", "rq3-attritioned-vs.-original-means-variances-correlations", "Unadjusted/Weighted/Imputed vs. Original"),
    ("section", "9_rq6_combined_bias_reduction_est.html", "bias-reduction-ip-weighted-vs-unadjusted-attritioned-estimates", "Weighted/Imputed vs. Unadjusted"),
    ("heading", "ACE Estimates", 2),
    ("section", "9_rq6_combined_bias_reduction_est.html", "rq3-comparison-of-unadjustedweightedimputed-attritioned-estimates-to-original-estimates", "Unadjusted/Weighted/Imputed vs. Original"),
    ("image", "plots/pngs/9_ace_bias_conditions.png", "RQ3 ACE Estimates: Comparison of bias reduction conditions", "ACE Bias Plot"),

    ("heading", "Discussion", 1),
    ("section", "11_descriptives.html", "relationship-between-maternal-education-and-child-outcomes", "Maternal Education & Child Outcomes"),
    ("section", "2_rq2_results.html", "distribution-of-maternal-education-full-sample-vs.-y26-webtest-participants", "Maternal Education Distribution"),
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
            _, filename, section_id = entry[:3]
            label = entry[3] if len(entry) > 3 else section_id.replace("-", " ").title()
            indent = "&nbsp;" * current_level * 4
            items.append(f'<li>{indent}<a href="#{section_id}">{label}</a></li>')
        elif entry[0] == "image":
            _, filepath, caption, toc_label = entry
            anchor = re.sub(r"[^a-z0-9]+", "-", toc_label.lower()).strip("-")
            indent = "&nbsp;" * current_level * 4
            items.append(f'<li>{indent}<a href="#{anchor}">{toc_label}</a></li>')
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
            _, filename, section_id = entry[:3]
            try:
                soup = load_soup(filename, cache)
                html = extract_section(soup, section_id)
                body_parts.append(f'<div class="extracted-section" data-source="{filename}#{section_id}">{html}</div>')
                print(f"  OK  {filename}#{section_id}")
            except FileNotFoundError as e:
                body_parts.append(f'<div class="missing-section"><em>{e}</em></div>')
                print(f"  MISSING  {e}")

        elif kind == "image":
            _, filepath, caption, toc_label = entry
            anchor = re.sub(r"[^a-z0-9]+", "-", toc_label.lower()).strip("-")
            img_path = BASE_DIR / filepath
            if img_path.exists():
                body_parts.append(
                    f'<div class="extracted-section" id="{anchor}">'
                    f'<figure><img src="{filepath}" alt="{caption}" style="max-width:100%;">'
                    f'<figcaption>{caption}</figcaption></figure></div>'
                )
                print(f"  OK  {filepath}")
            else:
                body_parts.append(f'<div class="missing-section"><em>Image not found: {filepath}</em></div>')
                print(f"  MISSING  {filepath}")

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
    #toc {{ position: fixed; top: 20px; right: 20px; width: 360px; max-height: 85vh; overflow-y: auto;
            background: #f8f9fa; padding: 12px 16px; border-radius: 6px; z-index: 100;
            font-size: 0.82em; box-shadow: 0 2px 8px rgba(0,0,0,0.12); line-height: 1.6; }}
    #toc ul {{ list-style: none; padding-left: 0; margin: 0; }}
    #toc a {{ color: #3498db; text-decoration: none; }}
    #toc a:hover {{ text-decoration: underline; }}
    @media (max-width: 1280px) {{ #toc {{ display: none; }} }}
    #toc-inline {{ background: #f8f9fa; padding: 16px 20px; border-radius: 6px; margin: 20px 0;
                   font-size: 0.88em; line-height: 1.7; border-left: 4px solid #3498db; }}
    #toc-inline ul {{ list-style: none; padding-left: 0; margin: 0; }}
    #toc-inline a {{ color: #3498db; text-decoration: none; }}
    #toc-inline a:hover {{ text-decoration: underline; }}
    h1 {{ border-bottom: 2px solid #3498db; padding-bottom: 8px; }}
    h2, h3 {{ color: #2c3e50; }}
  </style>
</head>
<body>
  <h1 style="font-size:1.8em;">Supplementary Materials for TEDS Attrition Bias Paper</h1>
  <div id="toc-inline">
    <strong>Contents</strong>
    {toc_html}
  </div>
  <div id="toc">
    <strong>Contents</strong>
    {toc_html}
  </div>
  <hr>
  {"<hr>".join(body_parts)}
</body>
</html>"""

    out_path = BASE_DIR / "0_supplement.html"
    with open(out_path, "w", encoding="utf-8") as f:
        f.write(combined_html)
    print(f"\nWritten to: {out_path}")


if __name__ == "__main__":
    main()
