export const slugify = (x: string = '') =>
  x
    .split(' ')
    .map((x) => x.normalize('NFKD').replace(/[^\w]/g, '').toLowerCase())
    .join('-');

export const fromSlugArray = (slug: undefined | string | string[]): string =>
  !slug ? '' : Array.isArray(slug) ? slug.join('/') : slug;

export const arrayFromSlug = (slug: undefined | string | string[]): string[] =>
  !slug ? [] : Array.isArray(slug) ? slug : [slug];

export const numberFromSlug = (slug: undefined | string | string[]): number =>
  parseInt(fromSlugArray(slug), 10);
