import { SubmitFormDocument } from '@app/graphql/Crm';
import { RichTextEditor } from '@app/ui/RichTextEditor';
import { Dialog, DialogContent, DialogTrigger, DialogTitle } from '@app/ui/dialog';
import { TextFieldElement } from '@app/ui/fields/text';
import { SubmitButton } from '@app/ui/submit';
import { useCookie } from '@app/ui/use-cookie';
import cx from 'classnames';
import { Edit3 } from 'lucide-react';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';

function FeedbackForm() {
  const [isOpen, setIsOpen] = React.useState(false);
  const { control, handleSubmit } = useForm();
  const submit = useMutation(SubmitFormDocument)[1];
  const [isSubmitted, setSubmitted] = useCookie('feedback-submitted', 'true');

  const onSubmit = useAsyncCallback(async (data: any) => {
    const url = window.location.toString();
    await submit({ type: 'Zpětná vazba, web 05/2023', data, url });
    setIsOpen(false);
    setSubmitted('true', 30);
    toast.success('Děkujeme za odezvu!');
  });

  if (!!isSubmitted) {
    return <div key="feedback" />;
  }

  return (
    <div key="feedback" className="absolute bottom-4 right-4">
      <Dialog open={isOpen} onOpenChange={setIsOpen}>
        <DialogTrigger asChild>
          <button
            className={cx(
              'text-white shadow-stone-700 bg-red-500 hover:bg-red-600 inline-flex h-[35px] items-center justify-center rounded-lg px-3 leading-none shadow-[0_2px_10px] focus:shadow-[0_0_0_2px] focus:shadow-black focus:outline-none',
            )}
          >
            <Edit3 className="h-4 w-4 mr-2" /> Ohodnoť nový web!
          </button>
        </DialogTrigger>

        <DialogContent>
          <DialogTitle className="text-neutral-12 mb-2 text-lg">
            <Edit3 className="inline h-4 w-4 mr-1" /> Ohodnoť nový web!
          </DialogTitle>

          <form className="grid gap-4" onSubmit={handleSubmit(onSubmit.execute)}>
            <TextFieldElement control={control} name="name" label="Jméno" />
            <TextFieldElement control={control} name="email" type="email" placeholder="@" label="E-mail" />
            <RichTextEditor
              control={control}
              name="feedback"
              label="Nedostatky, připomínky, chvála, ..."
            />
            <div className="flex justify-end">
              <SubmitButton loading={onSubmit.loading} />
            </div>
          </form>
        </DialogContent>
      </Dialog>
    </div>
  );
}

export default FeedbackForm;
