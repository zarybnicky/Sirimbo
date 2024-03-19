import classNames from 'classnames';
import { twMerge } from 'tailwind-merge';

export const cn: ((...inputs: classNames.ArgumentArray) => string) = (...inputs) => twMerge(classNames(...inputs));
