import { z } from 'zod';

export const mifareLabelSchema = z
  .string()
  .trim()
  .superRefine((value, ctx) => {
    if (!/^(0|[1-9][0-9]*)$/.test(value)) {
      ctx.addIssue({
        code: 'custom',
        message: 'Zadejte desítkové UID uvedené na kartě.',
      });
      return;
    }
    if (value.length > 10 || (value.length === 10 && value > '4294967295')) {
      ctx.addIssue({ code: 'custom', message: 'UID musí být 32bitové číslo.' });
    }
  });

export const mifareCodeSchema = z
  .string()
  .trim()
  .regex(/^[0-9A-Fa-f]{8}$/, 'UID čtečky musí mít osm hexadecimálních znaků.');

export function mifareLabelToCode(label: string): string {
  const value = mifareLabelSchema.parse(label);
  return BigInt(value)
    .toString(16)
    .padStart(8, '0')
    .match(/../g)!
    .toReversed()
    .join('')
    .toUpperCase();
}

export function mifareCodeToLabel(code: string): string {
  const value = mifareCodeSchema.parse(code).toUpperCase();
  const hex = value.match(/../g)!.toReversed().join('');
  return BigInt(`0x${hex}`).toString(10);
}
