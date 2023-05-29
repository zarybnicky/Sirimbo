import classNames from 'clsx';
import { twMerge } from 'tailwind-merge';

export const cn: typeof classNames = (...inputs) => twMerge(classNames(...inputs));

export function absoluteUrl(path: string) {
  return `${process.env.NEXT_PUBLIC_APP_URL}${path}`
}
