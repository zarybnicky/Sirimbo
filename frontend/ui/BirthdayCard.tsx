import React from 'react';
import Confetti from 'react-confetti';
import ConfettiExplosion from 'react-confetti-explosion';
import { Dialog, DialogContent } from './dialog';
import { buttonCls, typographyCls } from './style';

export function BirthdayCard() {
  const [status, setStatus] = React.useState<'run' | 'closing' | 'closed'>('run');
  const {width, height} = useWindowSize();

  if (status === 'closed') return;
  return (
    <div className="absolute inset-0">
      <Confetti height={height} width={width} recycle={status !== 'closing'} />
      <Dialog open={true} modal={false}>
        <DialogContent className="flex flex-col my-6 gap-1 items-center max-w-xs sm:max-w-xs">
          <div className={typographyCls({variant:'smallHeading', className: 'mb-0'})}>Congratulations,</div>
          <div className={typographyCls({variant:'heading', className: 'mb-0'})}>you&apos;ve aged!</div>
          <div className={typographyCls({variant:'label', className: 'my-3'})}>(platby už se chystají, neboj ;)</div>
          {status === 'closing' && <ConfettiExplosion {...{
              force: 0.6,
              duration: 2500,
              particleCount: 80,
              width,
              height,
              zIndex: 10000
            }} />}
          <button className={buttonCls()} type="button" onClick={() => {
            setStatus('closing');
            setTimeout(() => setStatus('closed'), 3000);
          }}>...stačilo konfet?</button>
        </DialogContent>
      </Dialog>
    </div>
  );
}
const useWindowSize = () => {
  const [windowSize, setWindowSize] = React.useState({ width: 0, height: 0 });
  const handleSize = () => setWindowSize({ width: window.innerWidth, height: window.innerHeight });
  React.useLayoutEffect(() => {
    handleSize();
    window.addEventListener("resize", handleSize);
    return () => window.removeEventListener("resize", handleSize);
  }, []);
  return windowSize;
};
