import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from '@app/ui/fields/text';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { UpozorneniInput } from '@app/graphql';
import {
  AnnouncementDocument,
  AnnouncementFragment,
  CreateAnnouncementDocument,
  DeleteAnnouncementDocument,
  UpdateAnnouncementDocument,
} from '@app/graphql/Announcement';
import { DatePickerElement } from '@app/ui/fields/date';
import { useMutation } from 'urql';
import { CheckboxElement } from './Checkbox';
import { DeleteButton } from './DeleteButton';
import { toast } from 'react-toastify';
import { useRouter } from 'next/router';
import { RichTextEditor } from './RichTextEditor';
import { TitleBar } from './layout/TitleBar';
import { Announcement } from 'lib/entities';
import { makeEntityFetcher } from './generic/WithEntity';

type FormProps = Pick<UpozorneniInput, 'upNadpis' | 'upText' | 'isVisible' | 'sticky'> & {
  scheduledSince: Date | undefined;
  scheduledUntil: Date | undefined;
};

const entity = Announcement;

export function AnnouncementForm({
  id,
  data,
}: {
  id?: string;
  data?: AnnouncementFragment | null;
}) {
  const router = useRouter();
  const title = id ? entity.title(data) : 'Nový příspěvek';

  const create = useMutation(CreateAnnouncementDocument)[1];
  const update = useMutation(UpdateAnnouncementDocument)[1];

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      upNadpis: data?.upNadpis,
      upText: data?.upText,
      isVisible: data?.isVisible,
      scheduledSince: data?.scheduledSince ? new Date(data.scheduledSince) : undefined,
      scheduledUntil: data?.scheduledUntil ? new Date(data.scheduledUntil) : undefined,
    });
  }, [data, reset]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const patch = {
      upNadpis: values.upNadpis,
      upText: values.upText,
      isVisible: values.isVisible,
      scheduledSince: values.scheduledSince?.toISOString(),
      scheduledUntil: values.scheduledUntil?.toISOString(),
    };
    if (id) {
      await update({ id, patch });
    } else {
      const res = await create({ input: patch });
      const id = res.data?.createUpozorneni?.upozorneni?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace(entity.editRoute(id));
      } else {
        reset(undefined);
      }
    }
  });

  return (
    <form className="space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TitleBar backHref={entity.listRoute} title={title}>
        {id && (
          <DeleteButton
            doc={DeleteAnnouncementDocument}
            id={id}
            title="smazat příspěvek"
            onDelete={() => router.push(entity.listRoute)}
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
      </TitleBar>

      <FormError error={onSubmit.error} />
      <TextFieldElement control={control} name="upNadpis" label="Nadpis" required />
      <DatePickerElement
        control={control}
        name="scheduledSince"
        label="Odložit zveřejnění na den"
      />
      <DatePickerElement
        control={control}
        name="scheduledUntil"
        label="Skrýt příspěvek dne"
      />
      <CheckboxElement control={control} name="isVisible" value="1" label="Viditelný" />
      <CheckboxElement
        control={control}
        name="sticky"
        value="1"
        label="Připnutý příspěvek (stálá nástěnka)"
      />
      <RichTextEditor
        initialState={data?.upText}
        control={control}
        name="upText"
        label="Text"
      />
    </form>
  );
}

AnnouncementForm.fetcher = makeEntityFetcher(AnnouncementDocument)((x) => x?.upozorneni);
