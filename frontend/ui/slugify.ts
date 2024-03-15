export const slugify = (x: string = '') =>
  x
    .split(' ')
    .map((x) => x.normalize('NFKD').replace(/[^\w]/g, '').toLowerCase())
    .join('-');
