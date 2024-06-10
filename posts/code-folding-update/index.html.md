---
title: An update on code folding with blogdown + Academic theme
date: '2020-05-03'
categories:
- programming
- Rstats
number-sections: true
code-tools: false
date-modified: '2024-06-08'
keep-md: true
---



::: callout-important
**2024-06-08** I have ported my website to Quarto, which has native support for code folding. I'm leaving this post up despite the fact that the functionality is no longer relevant for this website.

**2020-11-21** _Thanks to Allen O'Brien for pointing out a bug in the codefolding code, which led to the last code chunk defaulting to hidden rather than open. Allen sent along a simple fix to the [`codefolding.js`](/files/_codefolding/codefolding.js) file._
:::

About a year ago I added a code-folding feature to my site, following an approach developed by [Sébastien Rochette](https://statnmap.com/2017-11-13-enable-code-folding-in-bookdown-and-blogdown/). I recently updated my site to work with the latest version of the [Academic theme](https://sourcethemes.com/academic/) for Hugo, and it turns out that this broke [my code-folding implementation](/posts/code-folding-with-blogdown-academic/). It took a bit of putzing and some help from a freelance web developer to fix it, but it's now working again, and I'm again doing my happy robot dance:

![](https://media.giphy.com/media/mIZ9rPeMKefm0/giphy.gif)

In this post, I'll provide instructions on how to reproduce the approach with the current version of the Academic theme, which is [4.8 (March 2020)](https://sourcethemes.com/academic/updates/v4.8.0/). Credit where credit is due: 

- Sébastien Rochette worked out [the earlier implementation](https://statnmap.com/2017-11-13-enable-code-folding-in-bookdown-and-blogdown/).
- Web developer [Max B.](https://upwork.com/freelancers/~01328c0a21498eac2a) worked out the kinks to get it working with the latest version of Academic. We connected through Upwork. Hire him there if you have web dev work!
- As I've said before, I couldn't write javascript to save my life, and my only contribution here is to write down the instructions.

## Code folding with the Academic theme

1.  You'll first need to add the codefolding javascript assets. Create a folder called `js` under the `/static` directory of your site. Add the file [`codefolding.js`](/files/_codefolding/codefolding.js).

2.  Create a folder called `css` under the `/static` directory of your site. Add the file [`codefolding.css`](/files/_codefolding/codefolding.css). This is the css for the buttons that will appear on your posts. 

3.  Add the file [`article_footer_js.html`](/files/_codefolding/article_footer_js.html) to the `/layouts/partials` directory of your site.

4.  Add the file [`header_maincodefolding.html`](/files/_codefolding/header_maincodefolding.html) to the `/layouts/partials` directory of your site.

5.  If you do not already have a file `head_custom.html` in the `/layouts/partials` directory, create it. Add the following lines of code to the file:


    ::: {.cell}
    
    ```{.js .cell-code}
    {{ if not site.Params.disable_codefolding }}
      <script src="https://cdn.jsdelivr.net/npm/popper.js@1.16.0/dist/umd/popper.min.js"></script>
      <link rel="stylesheet" href="{{ "css/codefolding.css" | relURL }}" />
    {{ end }}
    ```
    :::


6.  If you do not already have a file `site_footer.html` in the `/layouts/partials` directory, copy it over from `/themes/hugo-academic/layouts/partials`. Add the following lines of code to it, somewhere towards the bottom:


    ::: {.cell}
    
    ```{.js .cell-code}
    <!-- Init code folding -->
    {{ partial "article_footer_js.html" . }}
    ```
    :::


7.  If you do not already have the file `page_header.html` in the `/layouts/partials` directory, copy it over from `/themes/hugo-academic/layouts/partials`. Add the following line of code at appropriate points so that your posts will include the "Show/hide code" button:


    ::: {.cell}
    
    ```{.js .cell-code}
    {{ partial "header_maincodefolding" . }}
    ```
    :::


    Note that you'll likely need to add it twice due do conditionals in `page_header.html`. For example, [my version of the file](/files/_codefolding/page_header.html) includes the partial at lines 62 and 91.
    
8.  Modify your `params.toml` file (in the directory `/config/_default`) to include the following lines:


    ::: {.cell}
    
    ```{.r .cell-code}
    ############################
    ## Code folding
    ############################
    
    # Set to true to disable code folding
    disable_codefolding = false
    # Set to "hide" or "show" all codes by default
    codefolding_show = "show"
    # Set to true to exclude the "Show/hide all" button
    codefolding_nobutton = false
    ```
    :::


## Using the codefolding parameters

The `params.toml` file now has three parameters that control code folding:

- `disable_codefolding` controls whether to load the code folding scripts on your site. Set it to `true` to disable code folding globally. 
- `codefolding_show` controls whether code blocks will be shown or hidden by default. If your previous posts have lots of code in them, set the default to `show` to minimize changes in the appearance of your site.
- `codefolding_nobutton` controls whether the "Show/hide code" button will appear at the top of posts that include code blocks. Set it to `true` to disable the button but keep the other code folding functionality. 

The above parameters are defaults for your entire site. To over-ride the defaults, you can also set the parameters in the YAML header of any post: 

- Set `disable_codefolding: true` to turn off code folding for the post.
- Set `codefolding_show: hide` to hide the code blocks in the post (as in [this post](/package-downloads/)).
- Set `codefolding_nobutton: true` to turn off the "Show/hide code" button at the top of the post (as in the present post).

I hope these instructions work for you. If not, questions, corrections, and clarifications are welcome. Happy blogging, y'all! 
