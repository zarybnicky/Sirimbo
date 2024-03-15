import { UpozorneniInput } from '@/graphql';
import {
  AnnouncementFragment,
  CreateAnnouncementDocument,
  UpdateAnnouncementDocument,
} from '@/graphql/Announcement';
import { CheckboxElement } from '@/ui/fields/checkbox';
import { DatePickerElement } from '@/ui/fields/date';
import { RichTextEditor } from '@/ui/fields/richtext';
import { TextFieldElement } from '@/ui/fields/text';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';

type FormProps = Pick<UpozorneniInput, 'upNadpis' | 'upText' | 'isVisible' | 'sticky'> & {
  scheduledSince: Date | undefined;
  scheduledUntil: Date | undefined;
};

export function AnnouncementForm({ id, data, onSuccess }: {
  id?: string;
  data?: AnnouncementFragment | null;
  onSuccess?: (id: string) => void;
}) {
  const create = useMutation(CreateAnnouncementDocument)[1];
  const update = useMutation(UpdateAnnouncementDocument)[1];

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      upNadpis: data?.upNadpis,
      upText: data?.upText,
      isVisible: data ? data.isVisible : true,
      sticky: data?.sticky,
      scheduledSince: data?.scheduledSince ? new Date(data.scheduledSince) : undefined,
      scheduledUntil: data?.scheduledUntil ? new Date(data.scheduledUntil) : undefined,
    });
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const patch = {
      upNadpis: values.upNadpis,
      upText: values.upText,
      isVisible: values.isVisible,
      sticky: values.sticky,
      scheduledSince: values.scheduledSince?.toISOString(),
      scheduledUntil: values.scheduledUntil?.toISOString(),
    };
    if (id) {
      await update({ id, patch });
      onSuccess?.(id);
    } else {
      const res = await create({ input: patch });
      const id = res.data?.createUpozorneni?.upozorneni?.id;
      if (id) {
        toast.success('Přidáno.');
        onSuccess?.(id);
      } else {
        reset(undefined);
      }
    }
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />

      <TextFieldElement control={control} name="upNadpis" label="Nadpis" required />
      <RichTextEditor initialState={data?.upText} control={control} name="upText" label="Text" />
      <CheckboxElement control={control} name="isVisible" value="1" label="Viditelný" />
      <CheckboxElement control={control} name="sticky" value="1" label="Připnout na stálou nástěnku" />
      <DatePickerElement control={control} name="scheduledSince" label="Odložit zveřejnění na den" />
      <DatePickerElement control={control} name="scheduledUntil" label="Skrýt příspěvek dne" />

      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
}
