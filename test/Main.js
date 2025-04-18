export function getData() {
  return ENV.generateData().toArray();
}

export function getTimeout() {
  return ENV.timeout;
}

export function pingRenderRate() {
  Monitoring.renderRate.ping();
}

export const setTimeoutImpl = (ms, fn) => setTimeout(fn, ms)

export function requestAnimationFrameImpl(f) {
  window.requestAnimationFrame(function () {
    f();
  });
}
