import classNames from 'classnames';
import { twMerge } from 'tailwind-merge';

export const cn: typeof classNames = (...inputs) => twMerge(classNames(...inputs));
