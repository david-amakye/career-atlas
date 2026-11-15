portrait <- function(role, skin, hair, glasses) {
  tone <- c(light = "#F5C6A3", warm = "#CA8960", brown = "#925A43", deep = "#684033")[[skin]]
  hair_path <- switch(hair,
    sweep = '<path d="M109 113 Q102 62 132 50 Q157 31 188 54 Q211 65 203 113 L190 89 Q162 99 146 74 Q140 95 119 105Z"/>',
    curls = '<path d="M108 112 Q95 91 110 79 Q94 60 121 57 Q119 37 144 47 Q158 29 174 46 Q194 36 197 59 Q222 59 207 82 Q221 97 202 111 L190 84 Q161 97 128 83Z"/>',
    close = '<path d="M108 106 Q100 51 153 47 Q204 47 207 104 L194 85 Q156 65 120 91Z"/>',
    none = "")
  specs <- if (isTRUE(glasses)) '<path d="M119 112 h30 v20 h-30z M164 112 h30 v20 h-30z M149 120 h15" fill="none" stroke="#26223f" stroke-width="3"/>' else ""
  props <- c(chart = '<path d="M14 62V47h10v15m9 0V36h10v26m9 0V24h10v38 M13 37l17-12 14 3 19-15"/>',
    code = '<path d="M26 29L15 40l11 11m25-22 11 11-11 11m-10-27-7 32"/>',
    design = '<rect x="15" y="20" width="48" height="38" rx="4"/><circle cx="27" cy="32" r="4"/><path d="M19 53l16-13 8 6 13-13 6 8"/>',
    plan = '<rect x="18" y="19" width="44" height="43" rx="5"/><path d="M18 31h44M29 14v12m22-12v12M27 47l8 6 16-14"/>',
    campaign = '<path d="M16 34h13l26-12v37L29 47H16z M23 47l6 15h9l-5-13 M62 34l7-5m-7 16 7 5"/>',
    shield = '<path d="M40 16l20 8v16q-2 14-20 25Q22 54 20 40V24z M31 40l6 7 13-15"/>',
    people = '<circle cx="40" cy="28" r="8"/><path d="M25 57v-5q0-12 15-12t15 12v5z M21 34a6 6 0 0 0 0 12m38-12a6 6 0 0 1 0 12"/>')
  sprintf(paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 420 298" role="img">',
    '<title>Career caricature: %s</title>',
    '<rect width="420" height="298" rx="20" fill="#eee9ff"/>',
    '<circle cx="159" cy="137" r="105" fill="#ded3ff"/>',
    '<path d="M22 206Q94 269 204 235T399 85" fill="none" stroke="#a28adf" stroke-width="3" stroke-dasharray="6 9"/>',
    '<g fill="#fff"><path d="M271 54l5 11 11 5-11 5-5 11-5-11-11-5 11-5z M74 79l4 7 8 4-8 4-4 8-4-8-8-4 8-4z"/></g>',
    '<path d="M69 247Q79 188 134 183h48q55 5 67 64z" fill="#45318b"/>',
    '<path d="M140 164h35v27q-18 22-35 0z" fill="%s"/>',
    '<ellipse cx="109" cy="130" rx="12" ry="15" fill="%s"/><ellipse cx="202" cy="130" rx="12" ry="15" fill="%s"/>',
    '<ellipse cx="156" cy="119" rx="48" ry="61" fill="%s"/>',
    '<g fill="#2d2238">%s<ellipse cx="136" cy="122" rx="4" ry="6"/><ellipse cx="177" cy="122" rx="4" ry="6"/></g>',
    '%s<path d="M140 148q16 14 33 0" fill="none" stroke="#572d41" stroke-width="3" stroke-linecap="round"/>',
    '<g transform="translate(274 111) rotate(9 40 40)"><rect width="80" height="80" rx="17" fill="#f9d96d"/>',
    '<g fill="none" stroke="#3e2b72" stroke-width="3" stroke-linecap="round" stroke-linejoin="round">%s</g></g>',
    '<rect x="17" y="251" width="386" height="31" rx="8" fill="#faf7ff"/>',
    '<text x="210" y="272" text-anchor="middle" fill="#382761" font-family="sans-serif" font-weight="700" font-size="14">%s</text></svg>'),
    role$title, tone, tone, tone, tone, hair_path, specs, props[[role$prop]], role$title)
}
