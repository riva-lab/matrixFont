/**
 * md2html - Simple Markdown2HTML end-mile converter.
 * Ready to actual use, no extra coding required.
 * Supports @@-beginning H6 sections in MD-file to create custom elements:
 *  - header;
 *  - menu;
 *  - TOC;
 *  - button bar;
 *  - image gallery;
 *  - themes switcher;
 *  - pages localization;
 *  - meta information for SEO;
 *  - structured data in LD/JSON & more.
 * Also supports:
 *  - header and footer from external file;
 *  - manual and auto switching between light and dark themes.
 * ---
 * Created by Riva, 2023-12-07.
 * Based on "Showdown" by ShowdownJS team https://github.com/showdownjs/showdown
 * Inspired by "markdown page" by Oscar Morrison https://github.com/oscarmorrison/md-page
 * MIT License.
 */

document.addEventListener("DOMContentLoaded", function () {
    // setPageDefaultStyle(); // Styles  
    setPageViewport();     // Viewport
    setPageEncoding();     // Encoding: utf-8
    updateColorScheme();

    switch (window.location.protocol) {

        // use this branch to load md content from external file
        case 'http:':
        case 'https:':
            loadContentExternal();
            break;

        // use this branch to load md content directly from template html file
        default:
            loadContentInternal();
    }
});


function setPageDefaultStyle() {
    var sheet = document.createElement('style');
    var styles = 'body { padding: 20px; font-family:  "Helvetica Neue", Helvetica, Arial, sans-serif;} ';
    styles += 'blockquote { padding: 0 1em; color: #6a737d; border-left: 0.25em solid #dfe2e5;} ';
    styles += 'code { padding: 0.2em 0.4em; background: rgba(27,31,35,0.05); border-radius: 3px;} ';
    styles += 'pre > code { background: none } ';
    styles += 'pre { padding: 16px; overflow: auto; line-height: 1.45; background-color: #f6f8fa; border-radius: 3px; } ';
    styles += 'table { border-collapse: collapse; } ';
    styles += 'td, th {  border: 1px solid #ddd; padding: 10px 13px; } ';
    sheet.innerHTML = styles;
    document.head.prepend(sheet);
}

function setPageViewport() {
    makeMetaTag('name', 'viewport', 'content', 'width=device-width, initial-scale=1, shrink-to-fit=no');
}

function setPageEncoding() {
    makeMetaTag('charset', 'UTF-8');
}

function checkSpecialPage(url, base) {
    if (url.match(base + '.md'))
        url = (new URL(base + '.md', window.location.origin)).href;

    return url;
}

function getMarkdownFilename() {
    var a = document.querySelector('link[rel="markdown"]');
    a = a ? a.getAttribute('href') : '';

    a = checkSpecialPage(a, '403');
    a = checkSpecialPage(a, '404');

    return a;
}

function loadContentExternal() {
    var client = new XMLHttpRequest();
    client.open('GET', getMarkdownFilename());
    client.send();

    client.onload = function () {
        makePage(convertMarkdownToHtml(client.responseText));
    }

    client.onerror = function () {
        loadContentInternal();
    }
}

function loadContentInternal() {
    var a = document.querySelector('noscript');
    if (a != null)
        makePage(convertMarkdownToHtml(a.innerText));
}

function convertMarkdownToHtml(markdown) {
    if (markdown == null) return;

    var converter = new showdown.Converter({
        emoji: true,
        underline: true,
    });
    converter.setFlavor('github');

    /**
     * Fix unexpected over-converting of char '<'
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /&amp;lt;/gi,
            replace: '\\&lt;'
        }];
    }, 'ltSignFix');

    /**
     * Fix unexpected over-converting of char '>'
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /&amp;gt;/gi,
            replace: '&gt;'
        }];
    }, 'gtSignFix');

    /**
     * Convert self-hosted links *.md to *.html
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /(<a\s[^>]*?href="[^"]+).md("[^>]*?>)/gi,
            replace: function (text) {
                var base = window.location.origin;
                if (window.location.protocol.startsWith('file:')) base = 'file:///';
                var url = (new URL(text.match(/"(.*?)"/)[1], base)).href;
                return url.includes(window.location.origin)
                    ? text.replace(/\.md"/gi, '.html"')
                    : text;
            }
        }];
    }, 'convertMdLinkToHtml');

    /**
     * Open links in new tab.
     * Self-hosted links are still opened in the same tab.
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /<a\shref[^>]+>/g,
            replace: function (text) {
                var url = text.match(/"(.*?)"/)[1];
                var loc = window.location.href;
                if (!loc.startsWith('file:///'))
                    url = (new URL(url, window.location.origin)).href;
                else
                    if (!url.startsWith('http://') && !url.startsWith('https://')) return text;
                if (url.includes(window.location.origin)) return text;
                return '<a href="' + url + '" target="_blank">';
            }
        }];
    }, 'externalLink');

    /**
     * Extension for converter to fix SVG rendering.
     * This code replace tag <img> with <embed> for .svg images.
     * This is necessary because of incorrect rendering png embedded in svg.
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /(<)(img)([^>]*src="[^"]*?screenshots\/[^"]*?.svg"[^>]*>)/gi,
            replace: '$1embed$3'
        }];
    }, 'fixSvg');

    /**
     * Simple XSS filter.
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /=\s*?"(javascript:[^"]*)"/gi,
            replace: '=""'
        }];
    }, 'preventJSExec');

    /**
     * IMG and EMBED tags wrapper
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /<p>(\s*?<(img|embed)\s.*?src="([^"]*?)".*?alt="([^"]*?)".*?)<\/p>/gi,
            replace: '<div class="div-$2"><a href="$3" target="_blank">$1</a><p>$4</p></div>'
        }];
    }, 'tagImgAndEmbedWrapper');

    /**
     * IMG and EMBED empty caption cleaner
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /(<div class="div-(?:img|embed)".*?)(?:<p>\s*?<\/p>)(<\/div>)/gi,
            replace: '$1$2'
        }];
    }, 'tagImgAndEmbedCleaner');

    /**
     * Parse sections like @@
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            filter: function (text, converter, options) {
                return parseSections(text);
            }
        }];
    }, 'parseSections');

    /**
     * TOC Generator
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            filter: function (text, converter, options) {
                return makeToc(text);
            }
        }];
    }, 'generateTOC');

    /**
     * ToDo checkboxes fix - trim beginning spaces
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /(<input[^>]*>)\s*/gi,
            replace: '$1'
        }];
    }, 'todoFix');

    /**
     * Add class="line-numbers" to tag <PRE>
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /(<pre[^>]*?)(>.*?<\/pre>)/gis,
            replace: '$1 class="line-numbers"$2'
        }];
    }, 'preTagLineNumbersForPrism');

    /**
     * Header tags wrapper for correct CSS rendering
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /(<h\d[^>]*?>)(.*?)(<\/h\d>)/gis,
            replace: '$1<span>$2</span>$3'
        }];
    }, 'headerTagsWrapper');

    /**
     * Replace EnDash
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /([\s>])--([ \t])/gis,
            replace: '$1–$2'
        }];
    }, 'replaceEnDash');

    /**
     * Replace EmDash
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /([\s>])---([ \t])/gis,
            replace: '$1—$2'
        }];
    }, 'replaceEmDash');

    return converter.makeHtml(markdown);
}

function makePage(html) {
    document.body.innerHTML = html;
    var a = document.body.firstElementChild;
    if (a != null)
        document.title = document.title || a.innerText.trim().substring(0, 100);
    a = document.querySelector('h1');
    if (a != null)
        a.setAttribute('class', 'caption');

    loadExternalSection(getLinkHref('markdown-menu', 'menu'), false, false);
    loadExternalSection(getLinkHref('html', 'header'), false, true);
    loadExternalSection(getLinkHref('html', 'footer'), true, true);
    try { Prism.highlightAll(); } catch { }
    updateColorScheme();

    document.querySelectorAll('div#sec-langs ul li p a').forEach(element => {
        element.removeAttribute('target');
    });

    languageVerify();
    if (isSeoEnabled()) seo();
}

/**
 * Color scheme handling.
 */

function getLinkHref(rel, type) {
    var a = document.querySelector('link[rel="' + rel + '"][type="' + type + '"]');
    return a ? a.getAttribute('href') : '';
}

function setPrismCSS(type) {
    var newCSS = getLinkHref('prism', type == 'dark' ? 'dark' : 'light');
    var oldCSS = getLinkHref('prism', type == 'dark' ? 'light' : 'dark');
    var a = document.querySelector('link[rel="stylesheet"][href="' + oldCSS + '"]');
    if (a) a.setAttribute('href', newCSS);
}

// https://stackoverflow.com/questions/56393880/how-do-i-detect-dark-mode-using-javascript
function setColorScheme(scheme) {
    var catDst = '';
    var catSrc = '';

    switch (scheme) {
        case 'dark':
            catDst = getLinkHref('catalog', 'dark');
            catSrc = getLinkHref('catalog', 'light');
            document.querySelector('html').removeAttribute('light-theme');
            break;

        // case 'light': break;
        default:
            catDst = getLinkHref('catalog', 'light');
            catSrc = getLinkHref('catalog', 'dark');
            document.querySelector('html').setAttribute('light-theme', '');
            break;
    }

    // change images source
    var re = RegExp('src="([^>"]*?)' + catSrc + '([^>"]*?)"', 'gi');
    var str = 'src="$1' + catDst + '$2"';
    document.body.innerHTML = document.body.innerHTML.replace(re, str);

    // change links reference
    re = RegExp('<a href="([^>"]*?)' + catSrc + '([^>"]*?)"', 'gi');
    str = '<a href="$1' + catDst + '$2"';
    document.body.innerHTML = document.body.innerHTML.replace(re, str);

    setPrismCSS(scheme);
}

function getPreferredColorScheme() {
    if (window.matchMedia)
        return window.matchMedia('(prefers-color-scheme: dark)').matches ?
            'dark' : 'light';
    return 'light';
}

function updateColorScheme() {
    if (getLinkHref('catalog', 'light') &&
        getLinkHref('catalog', 'dark')) {

        var t = localStorage.getItem('theme');
        if (t == null) t = '0';

        switch (t) {
            case '0':
                setThemeSwitch('🌍', 'System theme', getPreferredColorScheme());
                break;

            case '1':
                setThemeSwitch('🌙', 'Dark theme', 'dark');
                break;

            default:
                setThemeSwitch('☀️', 'Light theme', 'light');
                break;
        }
    }
}

function setThemeSwitch(icon, hint, theme) {
    var e = document.querySelector('#sec-themes');
    if (e == null)
        setColorScheme(getPreferredColorScheme());
    else {
        e.innerHTML = icon;
        e.setAttribute('title', hint);
        setColorScheme(theme);
    }
}

function themesSwithClick() {
    var t = localStorage.getItem('theme');
    if (t == null) t = 0;
    if (++t > 2) t = 0;
    localStorage.setItem('theme', t);

    updateColorScheme();
}

if (window.matchMedia) {
    var colorSchemeQuery = window.matchMedia('(prefers-color-scheme: dark)');
    colorSchemeQuery.addEventListener('change', updateColorScheme);
}


/**
 * TOC handling: generating Table of Content.
 */

function tocListOpen(level) {
    return '<ul class="toc-h' + level + '">';
}

function tocListClose() {
    return '</ul>';
}

function tocListItemOpen(level) {
    return '<li class="toc-h' + level + '">';
}

function tocListItemClose() {
    return '</li>';
}

function makeToc(html) {
    // check if TOC is present, if doesn't then do nothing
    var reTOC = /<p>\s*\[toc\]\s*<\/p>/gi;
    if (reTOC.exec(html) == undefined) return html;

    var re = /(<)(h(\d))(\s[^>]*id="([^>"]*?)"[^>]*>)(.*?)(<\/h\3>)/gi;
    var headerTags = String(html).match(re);

    var str = '';
    var prevLevel = 0;

    if (headerTags != null && headerTags.length) {
        for (let h of headerTags) {
            var level = parseInt(h.replace(re, '$3'));

            if (level == prevLevel) {
                str += tocListItemClose() + tocListItemOpen(level);
            }
            else
                if (level > prevLevel) {
                    while (level > prevLevel++) str += tocListOpen(prevLevel);
                    str += tocListItemOpen(level);
                } else
                    if (level < prevLevel) {
                        str += tocListItemClose();
                        while (level < prevLevel--) str += tocListClose();
                        str += tocListItemOpen(level);
                    }

            str += h.replace(re, '<a href="#$5">$6</a>');
            prevLevel = level;
        }

        str += tocListItemClose();
        while (prevLevel--) str += tocListClose();
    }

    // shift unused levels of TOC
    var s = str;
    for (let i = 1; i <= 6; i++)
        if (s.match('<li class="toc-h' + i + '">'))
            break;
        else
            for (let j = 2; j <= 6; j++)
                str = str.replace(
                    RegExp(' class="toc-h' + j + '">', 'gi'),
                    ' class="toc-h' + (j - 1) + '">');

    // make link to TOC on each header 
    re = /((<h(\d)\s[^>]*)>(.*?)(<\/h\3>))/gi;
    html = html.replace(re, '$2 class="toc-h"><a href="#toc">$4</a>$5');

    // insert TOC and return
    str = '<div id="toc" class="toc">' + str + '</div>';
    return html.replace(reTOC, '?#?#toc').replace('?#?#toc', str);
}


function parseSections(html) {
    html = '<h6>@@content</h6>' + html + '<h6>@@</h6>';
    var s = '';

    let x;
    var re = /<h6[^>]*?>@@([^<]*?)(?:=([^<]*?))?<\/h6>(.*?)(?=<h6[^>]*?>@@)/gis;
    while ((x = re.exec(html)) !== null) {
        var key = x[1].toLowerCase();
        var hdr = x[2];
        var txt = x[3];

        switch (key) {
            case 'meta':
                if (isSeoEnabled()) parseMeta(txt);
                break;

            case 'header':
                s += getSection('div', 'sec-header', hdr, '<div>' + txt + '</div>');
                break;

            case 'images':
                s += getSection('div', 'sec-images', '', parseImages(txt, hdr));
                break;

            case 'links':
                s += getSection('div', 'sec-links', hdr, parseLinks(txt));
                break;

            case 'toc':
                s += getSection('div', 'sec-toc', hdr, '<p>[TOC]</p>');
                break;

            case 'langs':
                s += getSection('div hidden', 'sec-langs', hdr, parseLangs(txt));
                break;

            case 'themes':
                s += getSection('div onclick="themesSwithClick()"', 'sec-themes', '', '');
                break;

            case 'menu':
                s += getSection('div', 'sec-menu', '', parseMenu(txt, hdr));
                break;

            case 'structured':
            case 'structureddata':
                makeTag(document.head, 'script', 'type', 'application/ld+json').innerHTML =
                    txt.replace(/<[^>]+?>/gis, '');
                break;

            case 'content':
            default:
                if (txt == '') break;
                s += getSection('article', 'sec-content', hdr, txt);
                break;
        }
    }

    return document.querySelector('#main') ? s : '<div id="main">' + s + '</div>';
}

function getSection(tag, id, h1, content) {
    return '<' + tag + getSectionId(id) + '>' + getSectionH1(h1) + content + '</' + tag + '>';
}

function getSectionH1(text) {
    return (text != undefined && text != '') ? '<h1>' + text + '</h1>' : '';
}

function getSectionId(text) {
    return (text != undefined && text != '') ? ' id=' + text : '';
}

var loadMax = 0;
var loaded = 0;
function loadExternalSection(filename, after, isHtml) {
    if (filename == undefined || filename == '') return;
    var client = new XMLHttpRequest();
    client.open('GET', filename);
    client.send();
    loadMax++;

    client.onload = function () {
        var s = document.body.innerHTML;
        var r = client.responseText;
        r = isHtml ? r : convertMarkdownToHtml(r);
        document.body.innerHTML = after ? s + r : r + s;
        if (++loaded == loadMax) externalSectionsLoaded();
    }
}

function externalSectionsLoaded() {
    moveSection('body>#sec-menu', '#header-bottom');
    makeTag(document.querySelector('#header-bottom'), 'div', 'empty', '');
    moveSection('#sec-themes', '#header-bottom');
    moveSection('#sec-langs', '#header-bottom');
    if (document.querySelector('#header-bottom').childElementCount > 2)
        document.querySelector('#header-bottom > div[empty=""]').remove();

    var a = document.querySelector('#header-bottom');
    if (a.querySelectorAll('#sec-menu,#sec-langs,#sec-themes').length == 0) a.remove();

    var a = document.querySelector('#sec-langs');
    if (a != null)
        a.removeAttribute('hidden');
}

function moveSection(element, dest) {
    var sec = document.querySelector(element);
    if (sec == null) return;
    var a = document.querySelector(dest)
    if (a != null)
        a.appendChild(sec);
}

function parseLinks(html) {
    return html.replace(
        /(<p>\s*?<a)([^>]*?>.*?)(?:\s*?@@(\w+))(<\/a>\s*?<\/p>)/gis,
        '$1 class="btn-$3"$2$4');
}

function parseLangs(html) {
    return html.replace(
        /(<p>\s*?<a)([^>]*?>.*?)(?:\s*?@@(?:(\w*);)?(.+?))(<\/a>\s*?<\/p>)/gis,
        '$1 title="$4" lng="$3"$2$5');
}

function parseImages(html, prevDir) {
    return prevDir
        ? html.replace(
            /(<img\s+src="[^"]+?)([^"\/]+)\.[^\."]+(")/gis,
            '$1' + prevDir + '/$2.jpg$3')
        : html;
}

var menuCnt = 0;
function parseMenu(html, hdr) {
    menuCnt++;
    var id = '"m' + menuCnt + '-inp-$3"';
    html = '<input type="checkbox" id="menu-' + menuCnt + '"><label for="menu-'
        + menuCnt + '">☰' + (hdr ? hdr : '') + '</label>' + html;
    return html
        .replace(
            /(<li>\s*?<a\s[^>]*?>)([^<]*?)(<\/a>\s*<img\s[^>]*?alt=")([^"]*?)("[^>]*?\/>)/gis,
            '$1$2$3$2" loading="lazy$5')
        .replace(
            /(<img [^>]*?>)/gis,
            '<div>$1</div>')
        .replace(
            /(<li>)(?:\s*<p>\s*)(<a[^>]*>)?([^<]*)(<\/a>)?(?:\s*<\/p>)/gis,
            '$1<input type="checkbox" id=' + id + '><label for=' + id + '>$2$3$4</label>')
        .replace(
            /((?:src|href)=")(?:\.\.\/)*([^"]*")/gis,
            '$1' + window.location.origin + '/$2');
}

function parseMeta(html) {
    let x;
    var re = /<p[^>]*?>([^=<]*?)\s*=\s*(.*?)<\/p>/gis;

    while ((x = re.exec(html)) !== null) {
        var key = x[1].toLowerCase();
        var txt = x[2];
        if (key == '' || txt == '') continue;

        switch (key) {
            case 'title':
                var node = document.createElement('title');
                node.innerHTML = txt;
                document.head.appendChild(node);
            case 'og:title':
                makeMetaTagByProp('og:title', txt);
                break;

            case 'description':
            case 'desc':
                makeMetaTagByName('description', txt);
            case 'og:description':
            case 'og:desc':
                makeMetaTagByProp('og:description', txt);
                break;

            case 'published':
            case 'article:published_time':
                makeMetaTagByProp('article:published_time', txt);
                break;

            case 'modified':
            case 'article:modified_time':
                makeMetaTagByProp('article:modified_time', txt);
                break;

            case 'image':
            case 'og:image':
                makeMetaTagByProp('og:image', new URL(txt, window.location.href));
                break;

            case 'site':
            case 'og:site_name':
                makeMetaTagByProp('og:site_name', txt);
                break;

            case 'type':
            case 'og:type':
                makeMetaTagByProp('og:type', txt);
                break;

            case 'url':
            case 'og:url':
                makeMetaTagByProp('og:url', txt);
                break;

            case 'twitter':
                makeMetaTagByName('twitter:site', '@' + txt);
                break;

            case 'robots':
                makeMetaTagByName('robots', txt);
                break;

            case 'lang':
            case 'language':
                var a = document.querySelector('html');
                if (a != null)
                    a.setAttribute('lang', txt);
                break;

            case 'key':
            case 'keywords':
                makeMetaTagByName('keywords', txt);
                break;

            case 'author':
                makeMetaTagByName('author', txt);
                makeMetaTagByName('publisher', txt);
                break;
        }
    }
}

var seoEn = false;
function isSeoEnabled() {
    if (!seoEn) {
        var tag = document.querySelector('seo');
        seoEn |= tag != null && tag.hasAttribute('enable');
    }
    return seoEn;
}

function seo() {
    makeLinkTag('rel', 'canonical', 'href', window.location.href);

    makeMetaTagByProp('og:title', document.title);
    makeMetaTagByProp('og:url', window.location.href);
    makeMetaTagByProp('og:type', 'website');

    makeMetaTagByName('twitter:card', 'summary');
    ['title', 'description', 'image', 'url'].forEach(s => {
        var a = document.querySelector('meta[property="og:' + s + '"]');
        makeMetaTagByName('twitter:' + s,
            a ? a.getAttribute('content') : '');
    });

    makeHrefLang();
}

function makeTag(parent, tag, a1, a1txt, a2, a2txt, a3, a3txt) {
    if (!parent || !tag) return null;
    var d = document.createElement(tag);
    if (a1) d.setAttribute(a1, a1txt);
    if (a2) d.setAttribute(a2, a2txt);
    if (a3) d.setAttribute(a3, a3txt);
    parent.appendChild(d);
    return d;
}

function makeLinkTag(a1, a1txt, a2, a2txt, a3, a3txt) {
    makeTag(document.head, 'link', a1, a1txt, a2, a2txt, a3, a3txt);
}
function makeMetaTag(a1, a1txt, a2, a2txt, a3, a3txt) {
    makeTag(document.head, 'meta', a1, a1txt, a2, a2txt, a3, a3txt);
}

function makeMetaTagByName(name, content) {
    if (document.querySelector('meta[name="' + name + '"]') == null)
        makeMetaTag('name', name, 'content', content);
}

function makeMetaTagByProp(prop, content) {
    if (document.querySelector('meta[property="' + prop + '"]') == null)
        makeMetaTag('property', prop, 'content', content);
}

function makeHrefLang() {
    document.querySelectorAll('div#sec-langs ul li p a').forEach(element => {
        var h = new URL(element.getAttribute('href'), window.location.href);
        var l = element.getAttribute('lng').toLowerCase();
        makeLinkTag('rel', 'alternate', 'href', h, 'hreflang', l);
    });
}

function languageVerify() {
    var arr = [];
    var doc = document.querySelector('html').getAttribute('lang');
    var langs = document.querySelectorAll('div#sec-langs ul li p a');
    if (langs.length == 0) return;
    langs.forEach((e, i) => {
        var l = e.getAttribute('lng');
        var re = RegExp('^/(' + l + '|' + doc + ')/', 'gi');
        var p = '/' + l + window.location.pathname.replace(re, '/');
        if (i == 0) p = p.replace(re, '/');
        e.setAttribute('href', window.location.origin + p);
        arr.push({
            'l': l,
            't': e.getAttribute('title'),
            'h': e.getAttribute('href'),
            'x': e.innerHTML
        });
    });
    var def = arr[0].h;
    makeLinkTag('rel', 'alternate', 'href', def, 'hreflang', 'x-default');
    arr.sort((a, b) => { return a.l > b.l; });
    langs.forEach((e, i) => {
        var a = arr[i];
        e.setAttribute('lng', a.l);
        e.setAttribute('title', a.t);
        e.setAttribute('href', a.h);
        e.innerHTML = a.x;
    });
    var loc = navigator.language.split('-')[0];
    arr.forEach(e => { if (e.l == loc) return def = e.h; });
    var cur = localStorage.getItem('lang');
    if (cur == null) {
        localStorage.setItem('lang', loc);
        document.location.href = def;
    } else
        if (cur != doc)
            localStorage.setItem('lang', doc);
}