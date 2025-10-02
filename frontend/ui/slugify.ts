export const slugify = (x = '') =>
  x
    .split(' ')
    .map((x) => x.normalize('NFKD').replaceAll(/[^\w]/g, '').toLowerCase())
    .join('-');
