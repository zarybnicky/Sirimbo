import React from 'react';
import {
  getLatestActiveClientPoint,
  replayActiveSelections,
} from './Selection';

type Axis = 'x' | 'y';
type ScrollTarget = HTMLElement | Window;

const edgeSize = 56;
const maxSpeed = 700;

export function useDragAutoScroll(
  rootRef: React.RefObject<HTMLElement | null>,
  enabled: boolean,
) {
  React.useEffect(() => {
    if (!enabled) return;

    let frame = 0;
    let previousTime = 0;

    const tick = (time: number) => {
      const root = rootRef.current;
      const point = getLatestActiveClientPoint();
      const elapsed = previousTime ? Math.min((time - previousTime) / 1000, 0.05) : 0;
      previousTime = time;

      if (root && point && elapsed > 0) {
        const rect = visibleRect(root);

        if (
          point.clientX >= rect.left &&
          point.clientX <= rect.right &&
          point.clientY >= rect.top - edgeSize &&
          point.clientY <= rect.bottom + edgeSize
        ) {
          const xSpeed = edgeSpeed(point.clientX, rect.left, rect.right);
          const ySpeed = edgeSpeed(point.clientY, rect.top, rect.bottom);

          const didScrollY = scrollAxis(root, 'y', ySpeed * elapsed);
          const didScrollX = scrollAxis(root, 'x', xSpeed * elapsed);

          if (didScrollY || didScrollX) replayActiveSelections();
        }
      }

      frame = requestAnimationFrame(tick);
    };

    frame = requestAnimationFrame(tick);
    return () => cancelAnimationFrame(frame);
  }, [enabled, rootRef]);
}

function edgeSpeed(position: number, min: number, max: number) {
  if (position < min + edgeSize) {
    const proximity = Math.min(1, Math.max(0, (min + edgeSize - position) / edgeSize));
    return -maxSpeed * proximity * proximity;
  }
  if (position > max - edgeSize) {
    const proximity = Math.min(1, Math.max(0, (position - (max - edgeSize)) / edgeSize));
    return maxSpeed * proximity * proximity;
  }
  return 0;
}

function scrollAxis(root: HTMLElement, axis: Axis, amount: number) {
  if (!amount) return false;

  const direction = amount < 0 ? -1 : 1;
  const target = findScrollTarget(root, axis, direction);
  if (!target) return false;

  const before = getScrollPosition(target, axis);
  setScrollPosition(target, axis, before + amount);
  return getScrollPosition(target, axis) !== before;
}

function findScrollTarget(root: HTMLElement, axis: Axis, direction: -1 | 1) {
  for (let node: HTMLElement | null = root; node; node = node.parentElement) {
    const overflow = axis === 'y'
      ? getComputedStyle(node).overflowY
      : getComputedStyle(node).overflowX;

    if (/(auto|scroll|overlay)/.test(overflow) && canScroll(node, axis, direction)) {
      return node;
    }
  }

  return canScroll(window, axis, direction) ? window : null;
}

function canScroll(target: ScrollTarget, axis: Axis, direction: -1 | 1) {
  const element = isWindow(target) ? document.scrollingElement : target;
  if (!element) return false;

  const position = axis === 'y' ? element.scrollTop : element.scrollLeft;
  const viewport = axis === 'y' ? element.clientHeight : element.clientWidth;
  const size = axis === 'y' ? element.scrollHeight : element.scrollWidth;

  return direction < 0 ? position > 0 : position + viewport < size - 1;
}

function getScrollPosition(target: ScrollTarget, axis: Axis) {
  if (isWindow(target)) return axis === 'y' ? window.scrollY : window.scrollX;
  return axis === 'y' ? target.scrollTop : target.scrollLeft;
}

function setScrollPosition(target: ScrollTarget, axis: Axis, value: number) {
  if (isWindow(target)) {
    window.scrollTo({
      top: axis === 'y' ? value : window.scrollY,
      left: axis === 'x' ? value : window.scrollX,
      behavior: 'auto',
    });
  } else if (axis === 'y') {
    target.scrollTop = value;
  } else {
    target.scrollLeft = value;
  }
}

function isWindow(target: ScrollTarget): target is Window {
  return target === window;
}

function visibleRect(node: HTMLElement) {
  const rect = node.getBoundingClientRect();
  return {
    top: Math.max(0, rect.top),
    right: Math.min(window.innerWidth, rect.right),
    bottom: Math.min(window.innerHeight, rect.bottom),
    left: Math.max(0, rect.left),
  };
}
