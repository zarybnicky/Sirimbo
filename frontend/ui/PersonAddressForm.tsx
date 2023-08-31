import { TextField, TextFieldElement } from '@app/ui/fields/text';
import { typographyCls } from '@app/ui/style';
import { useZodForm } from '@/lib/use-schema-form';
import React from 'react';
import { TypeOf, z } from 'zod';
import { toast } from 'react-toastify';
import { useAsyncCallback } from 'react-async-hook';
import { SubmitButton } from './submit';

const Form = z.object({
  street: z.string(),
  conscriptionNumber: z.string(),
  orientationNumber: z.string(),
  district: z.string(),
  city: z.string(),
  postalCode: z.string(),
  region: z.string(),
});

export const PersonAddressForm = () => {
  const { control, handleSubmit } = useZodForm(Form);

  const onSubmit = useAsyncCallback(async (data: TypeOf<typeof Form>) => {
    console.log(data);
    toast.success(JSON.stringify(data));
    /* const res = await create({ input: patch });
     * const id = res.data!.createEvent?.event?.id;
     * toast.success('Přidáno.');
     * setOpen(false);
     * router.replace(`/clenove/${id}`); */
  });

  return (
    <form className="grid lg:grid-cols-2 gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <legend className={typographyCls({ variant: 'label' })}>Trvalé bydliště</legend>
      <TextFieldElement control={control} name="street" label="Ulice" />
      <TextFieldElement control={control} name="conscriptionNumber" label="Č. popisné" />
      <TextFieldElement
        control={control}
        name="orientationNumber"
        label="Č. orientační"
      />
      <TextFieldElement control={control} name="district" label="Část města" />
      <TextFieldElement control={control} name="city" label="Město" />
      <TextFieldElement control={control} name="postalCode" label="PSČ" />
      <TextFieldElement control={control} name="region" label="Kraj" />
      <TextField label="Země" value="Česká republika" disabled />

      <div className="col-span-2">
        <SubmitButton className="w-full" loading={onSubmit.loading}>
          Vytvořit
        </SubmitButton>
      </div>
    </form>
  );
};
