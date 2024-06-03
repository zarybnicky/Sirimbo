export const slugify = (x = '') =>
  x
    .split(' ')
    .map((x) => x.normalize('NFKD').replace(/[^\w]/g, '').toLowerCase())
    .join('-');
