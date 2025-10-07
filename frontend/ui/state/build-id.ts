import { buildId } from '@/lib/build-id';
import { atom } from 'jotai';

export const buildIdAtom = atom(buildId);
