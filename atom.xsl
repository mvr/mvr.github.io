<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:atom="http://www.w3.org/2005/Atom">

  <xsl:output method="html" encoding="UTF-8" indent="yes"/>

  <xsl:template match="/">
    <html lang="en">
      <head>
        <meta charset="utf-8"/>
        <title><xsl:value-of select="atom:feed/atom:title"/> — Preview</title>
        <style>
          body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; margin: 2rem auto; max-width: 44rem; line-height: 1.5; padding: 0 1rem; }
          h1 { margin-bottom: 0.25rem; }
          .meta { color: #666; margin-bottom: 1.5rem; }
          article { margin-bottom: 2.5rem; }
          article h2 { margin-bottom: 0.25rem; }
          article time { color: #666; font-size: 0.9rem; }
          article .summary { margin-top: 0.75rem; }
        </style>
      </head>
      <body>
        <h1><xsl:value-of select="atom:feed/atom:title"/></h1>
        <div class="meta">
          <p>Previewing <code><xsl:value-of select="atom:feed/atom:id"/></code></p>
          <p>Updated <xsl:value-of select="atom:feed/atom:updated"/></p>
        </div>
        <xsl:for-each select="atom:feed/atom:entry">
          <article>
            <h2>
              <a href="{atom:link/@href}"><xsl:value-of select="atom:title"/></a>
            </h2>
            <p>
              <time datetime="{atom:updated}">
                <xsl:value-of select="atom:updated"/>
              </time>
            </p>
            <div class="summary">
              <xsl:choose>
                <xsl:when test="atom:content">
                  <xsl:value-of select="atom:content" disable-output-escaping="yes"/>
                </xsl:when>
                <xsl:when test="atom:summary">
                  <xsl:value-of select="atom:summary" disable-output-escaping="yes"/>
                </xsl:when>
              </xsl:choose>
            </div>
          </article>
        </xsl:for-each>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
