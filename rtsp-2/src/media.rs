pub struct Presentation {
    // aggregate_control_uri: ControlURI,
    // description: PresentationDescription,
    // media_streams: HashMap<ControlURI, Box<MediaStream>>
}

pub trait PresentationDescription {
    // fn aggregate_control_uri() -> ControlURI;
}

pub trait MediaStream {
    fn content_modification() -> MediaContentModification;
    // fn control_uri() -> ControlURI;
    fn retention() -> MediaRetention;
    // fn scale_factors() -> ScaleFactors;
    fn seeking() -> MediaSeeking;
}

pub enum MediaContentModification {
    Dynamic,
    Immutable,
    TimeProgressing,
}

pub enum MediaRetention {
    TimeDuration,
    TimeLimited,
    Unlimited, 
}

pub enum MediaSeeking {
    BeginningOnly,
    NoSeeking,
    RandomAccess,
}

pub enum MediaUsage {
    DynamicOnDemand,
    Live,
    LiveWithRecording,
    OnDemand,
}
