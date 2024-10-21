import { useForm, type UseFormProps } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import type { TypeOf, ZodSchema } from 'zod';

type UseZodFormProps<Z extends ZodSchema> = Exclude<UseFormProps<TypeOf<Z>>, 'resolver'>;

export const useZodForm = <Z extends ZodSchema>(schema: Z, formProps?: UseZodFormProps<Z>) => useForm({
  ...formProps,
  resolver: zodResolver(schema),
});
